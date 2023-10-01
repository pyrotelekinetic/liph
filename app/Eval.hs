module Eval (runEval) where

import Control.Monad.Except
import Control.Monad.State
import Parser (Sexp (..), Table, MyState, Error)

import Debug.Trace


raise :: String -> ExceptT Error (State Table) a
raise = throwError

sexp :: ExceptT Error (State Table) MyState -> ExceptT Error (State Table) Sexp
sexp = fmap snd

map' :: Monad m => (Sexp -> m Sexp) -> Sexp -> m Sexp
map' f (x := y) = (:=) <$> f x <*> map' f y
map' f x = f x

eval :: MyState -> ExceptT Error (State Table) Sexp
eval (t, e) = case e of
  FuncL f := x -> sexp $ f (t, x)
  MacroL f := x -> sexp $ f (t, x)
  AtomL a -> do
    a' <- getBind (t, AtomL a)
    eval (t, a')
  x := ys -> do
    x' <- eval (t, x)
    eval (t, x' := ys)
  x -> pure x

getBind :: MyState -> ExceptT Error (State Table) Sexp
getBind = \case
  ([], _) -> raise "Error: exhasted bindings"
  ((n, f) : ts, AtomL s)
    | n == s -> pure f
    | otherwise -> getBind (ts, AtomL s)

evalList :: MyState -> ExceptT Error (State Table) Sexp
evalList (t, xs) = map' (\x -> eval (t, x)) xs

-- Arithmetic --

-- sums a list of IntLs
plusL :: MyState -> ExceptT Error (State Table) MyState
plusL (t, xs) = do
  xs' <- evalList (t, xs)
  case xs' of
    IntL n := NilL -> pure (t, IntL n)
    IntL n1 := IntL n2 -> pure (t, IntL (n1 + n2))
    IntL n := ns -> do
      (_, ns') <- plusL (t, ns)
      plusL (t, IntL n := ns')
    z -> raise $ "Type Error: '+' takes Ints,'n  " ++ show z ++ " do not have type Int"

-- subtracts two IntLs
minusL :: MyState -> ExceptT Error (State Table) MyState
minusL (t, xs) = do
  xs' <- evalList (t, xs)
  case xs' of
    IntL n1 := IntL n2 := NilL -> pure (t, IntL (n1 - n2))
    z -> raise $ "Type Error: '-' takes two Ints, \n\t" ++ show z ++ " do not have type Int"

-- multiplies two IntLs
multiplyL :: MyState -> ExceptT Error (State Table) MyState
multiplyL (t, xs) = do
  xs' <- evalList (t, xs)
  case xs' of
    IntL n := NilL -> pure (t, IntL n)
    IntL n1 := IntL n2 -> pure (t, IntL (n1 * n2))
    IntL n := ns -> do
      (_, ns') <- multiplyL (t, ns)
      multiplyL (t, IntL n := ns')
    z -> raise $ "Type Error: '*' takes IntLs,\n\t"++ show z ++ " do not have type Int"

-- divides two IntLs
divideL :: MyState -> ExceptT Error (State Table) MyState
divideL (t, xs) = do
  xs' <- evalList (t, xs)
  case xs' of
    IntL _ := IntL 0 := NilL -> raise "Please do not divide by zero"
    IntL n1 := IntL n2 := NilL -> pure (t, IntL (div n1 n2))
    z -> raise $ "Type Error: '/' takes two IntLs,\n\t" ++ show z ++ " do not have type Int"

-- negates one IntL
negativeL :: MyState -> ExceptT Error (State Table) MyState
negativeL (t, xs) = do
  xs' <- eval (t, xs)
  case xs' of
    IntL n -> pure (t, IntL (- n))
    z -> raise $ "Type Error: 'neg' takes one IntL\n\t" ++ show z ++ " does not have type Int"

arithmetics :: Table
arithmetics =
  [ ("+", FuncL plusL)
  , ("-", FuncL minusL)
  , ("*", FuncL multiplyL)
  , ("/", FuncL divideL)
  , ("neg", FuncL negativeL)
  ]


-- Let --

-- defines a let expression
letL :: MyState -> ExceptT Error (State Table) MyState
letL = \case
  (t, AtomL n :=  x := e := NilL) -> do
    e' <- eval (t, e)
    pure ((n, x) : t, e')
  _ -> raise "Syntax Error: Invalid let expression"

extend :: Sexp -> Sexp -> Table -> Maybe Table
extend NilL NilL t = Just t
extend NilL _ _ = Nothing
extend _ NilL _ = Nothing
extend (AtomL x := xs) (v := vs) t = ((x, v) :) <$> extend xs vs t

-- defines a lambda expression
lambdaL :: MyState -> ExceptT Error (State Table) MyState
lambdaL = \case
  (t, xs := ds := NilL) -> do
    fn' <- eval (t, FuncL fn)
    pure (t, fn')
      where
      fn (t', es) = do
        vs <- evalList (t', es)
        case extend xs vs t of
          Just t -> do
            ds' <- eval (t, ds)
            pure (t, ds')
          Nothing -> raise "Error: incorrect number of args in lambda expression"
  _ -> raise "Syntax Error: invalid lambda expression"

-- defines a recursive lambda expression
fixL :: MyState -> ExceptT Error (State Table) MyState
fixL = \case
  (t, AtomL f := xs := d := NilL) -> do
    fn' <- eval (t, FuncL fn)
    pure (t, fn')
    where
    fn (t', es) = do
      vs <- evalList (t', es)
      case extend xs vs ((f, FuncL fn) : t) of
        Just t -> do
          d' <- eval (t, d)
          pure (t, d')
        Nothing -> raise "Error: incorrect number of args in fix expression"
  _ -> raise "Error: invalid fix expression"

lets :: Table
lets =
  [ ("let", FuncL letL)
  , ("lambda", FuncL lambdaL)
  , ("fix", FuncL fixL)
  ]


-- Bool --

equalsL :: MyState -> ExceptT Error (State Table) MyState
equalsL (t, x) = case x of
  _ := NilL -> raise "Type Error: '=' takes two arguments"
  a := b := NilL -> do
    a' <- eval (t, a)
    b' <- eval (t, b)
    pure (t, BoolL $ a' == b')

andL :: MyState -> ExceptT Error (State Table) MyState
andL (t, x) = do
  x' <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL True -> pure (t, BoolL True)
    BoolL _ := BoolL _ -> pure (t, BoolL False)
    _ -> raise "Type Error: 'and' takes two Bools"

orL :: MyState -> ExceptT Error (State Table) MyState
orL (t, x) = do
  x' <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL _ -> pure (t, BoolL True)
    BoolL _ := BoolL True -> pure (t, BoolL True)
    BoolL False := BoolL False -> pure (t, BoolL False)
    _ -> raise "Type Error: 'or' takes two Bools"

xorL :: MyState -> ExceptT Error (State Table) MyState
xorL (t, x) = do
  x' <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL False -> pure (t, BoolL True)
    BoolL False := BoolL True -> pure (t, BoolL True)
    BoolL _ := BoolL _ -> pure (t, BoolL False)
    _ -> raise "Type Error: 'xor' takes two Bools"

notL :: MyState -> ExceptT Error (State Table) MyState
notL (t, x) = do
  x' <- eval (t, x)
  case stripNilL x' of
    BoolL True -> pure (t, BoolL False)
    BoolL False -> pure (t, BoolL True)
    _ -> raise "Type Error: 'not' takes one Bool"

ifL :: MyState -> ExceptT Error (State Table) MyState
ifL (t, p := d := e := NilL) = do
  p' <- eval (t, p)
  case stripNilL p' of
    BoolL True -> do
      d' <- eval (t, d)
      pure (t, d')
    BoolL False -> do
      e' <- eval (t, e)
      pure (t, e')
    _ -> raise "Type Error: 'if' takes one Bool and two expressions"

bools :: Table
bools =
  [ ("#t", BoolL True)
  , ("#T", BoolL True)
  , ("#f", BoolL False)
  , ("#F", BoolL False)
  , ("=", FuncL equalsL)
  , ("and", FuncL andL)
  , ("or", FuncL orL)
  , ("xor", FuncL xorL)
  , ("not", FuncL notL)
  , ("if", FuncL ifL)
  ]

letMacroL :: MyState -> ExceptT Error (State Table) MyState
--letMacroL = \case
--  (t, AtomL n := x := e := NilL) -> do
--    pure ((n, x) : t, e)
--  _ -> raise "Syntax Error: Invalid let-macro expression"
letMacroL (_, NilL) = raise "NilL"
letMacroL (_, _ := _ := NilL) = raise "length 2"
letMacroL (_, _ := _ := _ := NilL) = raise "length 3"
--letMacroL state@(t, AtomL n := x := e := NilL) = trace (show state) $ do
--  pure ((n, x) : t, e)
letMacroL (_, s) = trace (show $ sexpLength s) raise "Syntax Error: Invalid let-macro expression"

sexpLength :: Sexp -> Int
sexpLength NilL = 0
sexpLength (_ := b) = 1 + sexpLength b
sexpLength _ = 1

macros :: Table
macros = [("let-macro", MacroL letMacroL)]


builtins :: Table
builtins = arithmetics ++ bools ++ lets ++ macros

stripNilL :: Sexp -> Sexp
stripNilL = \case
  x := NilL -> stripNilL x
  NilL := x -> stripNilL x
  x := y -> stripNilL x := stripNilL y
  x -> x

runEval :: Sexp -> Table -> Either Error Sexp
runEval x = evalState . runExceptT $ run x
  where
  run :: Sexp -> ExceptT Error (State Table) Sexp
  run x = do
    x' <- eval (builtins, x)
    pure $ stripNilL x'
