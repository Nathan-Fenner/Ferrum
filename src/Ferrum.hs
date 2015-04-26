
{-# OPTIONS_GHC -w #-}

import Message
import Parse.Core
import Syntax.Module
import Parse.Module
import Location
import Lex

import Verify
import Verify.Kind.Module

import Verify.Kind

main :: IO ()
main = do
	source <- readFile "test.txt"
	let lexed = lexer "test.txt" source
	putStrLn $ concat $ map (++ "\n") $ map show lexed
	report parseModule $ lexer "test.txt" source
	return ()

report :: Parse Module -> [Locate Token] -> IO ()
report parser tokens = case run parser tokens of
	(_, Right parseModule) -> do
		print parseModule
		putStrLn $ "Verifying module:"
		print $ verifyModule parseModule
	(count, Left (Message message)) -> do
		print $ (displayLocation $ at $ head $ drop count tokens) ++ "   " ++ message
		putStrLn $ concat $ map (++"\n") $ map show $ take 10 $ drop count tokens

