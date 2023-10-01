import Parser (parse)
import Eval (runEval)

main :: IO ()
main = do
--  putStrLn "$ "
  i <- getLine
  putStrLn $ "$ " ++ i
  case parse i of
    Left e -> putStrLn e
    Right i' -> do
      putStrLn $ "parsed as: " ++ show i'
      case runEval i' [] of
        Left e -> putStrLn e
        Right i'' -> putStrLn $ "> " ++ show i''
  main
