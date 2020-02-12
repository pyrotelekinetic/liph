import Parser

parse :: String -> Sexp
parse = unwrap . finishedP . runParser sexpP

main = do
  input <- getLine
  print $ parse input
  print $ runEval $ parse input
  main
