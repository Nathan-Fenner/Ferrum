
module Lex(lexer,Token(TWord,TString,TInt,TDouble,TOperator,TSpecial)) where

import Location

data Token = TWord String | TString String | TInt Int | TDouble Double | TOperator String | TSpecial String
	deriving (Show, Eq)

tabSize :: Int
tabSize = 4

startsWith :: String -> String -> Bool
x `startsWith` y = take (length y) x == y

specialWords :: [String]
specialWords = ["var","if","while"]

lexer :: FilePath -> String -> [Locate Token]
lexer file source = go (1,1) source where
	go pos [] = []
	go pos@(line,col) cs@(ch : ct)
		|isLetter ch = con
			pos
			(\name -> if name `elem` ops then TOperator name else if name `elem` specialWords then TSpecial name else TWord name)
			isLetter
			cs
		|ch `elem` "[](){}.:;" = Locate (Location (file, line, col)) (TSpecial [ch]) : go (line,col+1) ct
		|isDigit ch = con pos (TInt . read) isDigit cs
		|ch == ' ' = go (line,col+1) ct
		|ch == '\n' = go (line+1, 1) ct
		|ch == '\r' = go (line, 1) ct
		|ch == '\t' = go (line, (div (col-1) tabSize + 1) * tabSize + 1) ct
		|ch == '"' = goString pos (line, col+1) "" ct
		-- otherwise, operator
		-- take the first operator that starts the string
		|otherwise = case filter (cs `startsWith`) ops of
			[] -> error $ "not a valid operator at " ++ show pos
			(op :_) -> Locate (Location (file, line, col)) (TOperator op) : go (line, col+length op) (drop (length op) cs)
		where
		ops = ["==",">=","<=",">","<","~=","%","++","+","*","-","/","and","or","not", "="]
	goString start pos sofar [] = error "parsing string literal but reached end of file"
	goString start (line, col) sofar ('\\':c:ct)
		|c == 'n' = next "\n"
		|c == 't' = next "\t"
		|c == 'r' = next "\r"
		|c == '\\' = next "\\"
		|c == 'v' = next "\v"
		|c == '\a' = next "\a"
		|c == '\b' = next "\b"
		|c == '"' = next "\""
		|c == '\'' = next "\'"
		|otherwise = error "expected some valid escape symbol after \\ in string literal"
		where
		next sym = goString start (line, col+2) (sofar ++ sym) ct
	goString (startLine, startCol) (line, col) sofar ('"' : ct) = Locate (Location (file, startLine, startCol)) (TString sofar)
		: go (line, col+1) ct
	goString start (line, col) sofar (ch:ct) = goString start (line, col+1) (sofar ++ [ch]) ct


	con pos@(line,col) fun pred cs = location (fun word) : go (line,col + length word) (dropWhile pred cs) where
		word = takeWhile pred cs
		location = Locate (Location (file, line, col))


isLetter :: Char -> Bool
isLetter x = x `elem` ['a'..'z'] ++ ['A'..'Z'] ++ "_#"

isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']
