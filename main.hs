{-# OPTIONS_GHC -Wno-tabs #-}

import Parser (parse)
import Eval (runEval)
import Control.Monad.Except

main :: IO ()
main = do
--	putStrLn "$ "
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
