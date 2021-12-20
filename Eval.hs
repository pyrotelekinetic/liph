{-# OPTIONS_GHC -Wno-tabs #-}

{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Control.Monad.State
import Parser (Sexp (..), Table, MyState, Error)

import Debug.Trace


raise :: String -> ExceptT Error (State Table) a
raise = throwError

table :: ExceptT Error (State Table) MyState -> ExceptT Error (State Table) Table
table = fmap fst

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
	x -> return x

getBind :: MyState -> ExceptT Error (State Table) Sexp
getBind = \case
	([], _) -> raise "Error: exhasted bindings"
	((n, f) : ts, AtomL s)
		| n == s -> return f
		| otherwise -> getBind (ts, AtomL s)

evalList :: MyState -> ExceptT Error (State Table) Sexp
evalList (t, xs) = do
	xs' <- map' (\x -> eval (t, x)) xs
	return xs'


-- Arithmetic --

-- sums a list of IntLs
plusL :: MyState -> ExceptT Error (State Table) MyState
plusL (t, xs) = do
	xs' <- evalList (t, xs)
	case xs' of
		IntL n := NilL -> return (t, IntL n)
		IntL n1 := IntL n2 -> return (t, IntL (n1 + n2))
		IntL n := ns -> do
			(_, ns') <- plusL (t, ns)
			plusL (t, IntL n := ns')
		z -> raise $ "Type Error: '+' takes Ints,'n  " ++ show z ++ " do not have type Int"

-- subtracts two IntLs
minusL :: MyState -> ExceptT Error (State Table) MyState
minusL (t, xs) = do
	xs' <- evalList (t, xs)
	case xs' of
		IntL n1 := IntL n2 := NilL -> return (t, IntL (n1 - n2))
		z -> raise $ "Type Error: '-' takes two Ints, \n\t" ++ show z ++ " do not have type Int"

-- multiplies two IntLs
multiplyL :: MyState -> ExceptT Error (State Table) MyState
multiplyL (t, xs) = do
	xs' <- evalList (t, xs)
	case xs' of
		IntL n := NilL -> return (t, IntL n)
		IntL n1 := IntL n2 -> return (t, IntL (n1 * n2))
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
		IntL n1 := IntL n2 := NilL -> return (t, IntL (div n1 n2))
		z -> raise $ "Type Error: '/' takes two IntLs,\n\t" ++ show z ++ " do not have type Int"

-- negates one IntL
negativeL :: MyState -> ExceptT Error (State Table) MyState
negativeL (t, xs) = do
	xs' <- eval (t, xs)
	case xs' of
		IntL n -> return (t, IntL (- n))
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
	(t, AtomL n :=	x := e := NilL) -> do
		e' <- eval (t, e)
		return ((n, x) : t, e')
	_ -> raise "Syntax Error: Invalid let expression"

extend :: Sexp -> Sexp -> Table -> Maybe Table
extend NilL NilL t = Just t
extend NilL vs _ = Nothing
extend xs NilL _ = Nothing
extend (AtomL x := xs) (v := vs) t = ((x, v) :) <$> extend xs vs t

-- defines a lambda expression
lambdaL :: MyState -> ExceptT Error (State Table) MyState
lambdaL = \case
	(t, xs := ds := NilL) -> do
		fn' <- eval (t, FuncL fn)
		return (t, fn')
			where
			fn (t', es) = do
				vs <- evalList (t', es)
				case extend xs vs t of
					Just t -> do
						ds' <- eval (t, ds)
						return (t, ds')
					Nothing -> raise "Error: incorrect number of args in lambda expression"
	_ -> raise "Syntax Error: invalid lambda expression"

-- defines a recursive lambda expression
fixL :: MyState -> ExceptT Error (State Table) MyState
fixL = \case
	(t, AtomL f := xs := d := NilL) -> do
		fn' <- eval (t, FuncL fn)
		return (t, fn')
		where
		fn (t', es) = do
			vs <- evalList (t', es)
			case extend xs vs ((f, FuncL fn) : t) of
				Just t -> do
					d' <- eval (t, d)
					return (t, d')
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
	return (t, BoolL $ a' == b')

andL :: MyState -> ExceptT Error (State Table) MyState
andL (t, x) = do
	x' <- eval (t, x)
	case stripNilL x' of
		BoolL True := BoolL True -> return (t, BoolL True)
		BoolL _ := BoolL _ -> return (t, BoolL False)
		_ -> raise "Type Error: 'and' takes two Bools"

orL :: MyState -> ExceptT Error (State Table) MyState
orL (t, x) = do
	x' <- eval (t, x)
	case stripNilL x' of
		BoolL True := BoolL _ -> return (t, BoolL True)
		BoolL _ := BoolL True -> return (t, BoolL True)
		BoolL False := BoolL False -> return (t, BoolL False)
		_ -> raise "Type Error: 'or' takes two Bools"

xorL :: MyState -> ExceptT Error (State Table) MyState
xorL (t, x) = do
	x' <- eval (t, x)
	case stripNilL x' of
		BoolL True := BoolL False -> return (t, BoolL True)
		BoolL False := BoolL True -> return (t, BoolL True)
		BoolL _ := BoolL _ -> return (t, BoolL False)
		_ -> raise "Type Error: 'xor' takes two Bools"

notL :: MyState -> ExceptT Error (State Table) MyState
notL (t, x) = do
	x' <- eval (t, x)
	case stripNilL x' of
		BoolL True -> return (t, BoolL False)
		BoolL False -> return (t, BoolL True)
		_ -> raise "Type Error: 'not' takes one Bool"

ifL :: MyState -> ExceptT Error (State Table) MyState
ifL (t, p := d := e := NilL) = do
	p' <- eval (t, p)
	case stripNilL p' of
		BoolL True -> do
			d' <- eval (t, d)
			return (t, d')
		BoolL False -> do
			e' <- eval (t, e)
			return (t, e')
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
--	(t, AtomL n := x := e := NilL) -> do
--		return ((n, x) : t, e)
--	_ -> raise "Syntax Error: Invalid let-macro expression"
letMacroL (t, NilL) = raise "NilL"
letMacroL (t, (a := b := NilL)) = raise "length 2"
letMacroL (t, (a := b := c := NilL)) = raise "length 3"
--letMacroL state@(t, AtomL n := x := e := NilL) = trace (show state) $ do
--	return ((n, x) : t, e)
letMacroL (_, s) = trace (show $ sexpLength s) raise "Syntax Error: Invalid let-macro expression"

sexpLength :: Sexp -> Int
sexpLength NilL = 0
sexpLength (a := b) = 1 + sexpLength b
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
runEval x = evalState $ runExceptT $ run x
	where
	run :: Sexp -> ExceptT Error (State Table) Sexp
	run x = do
		x' <- eval (builtins, x)
		return $ stripNilL x'
