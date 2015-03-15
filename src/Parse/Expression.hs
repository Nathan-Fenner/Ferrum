
module Parse.Expression where

import Parse.Core
import Lex
import Location

type Expression = Locate ExpressionValue

data ExpressionValue
	= Name String
	| LiteralInt Int
	| LiteralString String
	| Operator Expression (Locate String) Expression
	| Prefix (Locate String) Expression
	deriving Show


parseAtom :: Parse Expression
parseAtom = do
	Locate at token <- ask message
	case token of
		TWord name -> return $ Locate at $ Name name
		TInt int -> return $ Locate at $ LiteralInt int
		TString string -> return $ Locate at $ LiteralString string
		_ -> crash message
	where
		message = Message "expected literal"

data OpTree = OpAtom Expression | OpBranch OpTree (Locate String) OpTree

parseProduct :: Parse Expression
parseProduct = parseInfixLeft ["*","/","%"] parseAtom

parsePrefix :: [String] -> Parse Expression -> Parse Expression
parsePrefix ops atom = do
	next <- peekMaybe
	case next of
		Just (Locate at (TOperator op)) -> if op `elem` ops then do
				advance 1 -- advance past this operator
				value <- parsePrefix ops atom
				return $ Locate at $ Prefix (Locate at op) value
			else
				atom

parseInfix' :: [String] -> Parse Expression -> Parse OpTree
parseInfix' ops atom = do
	left <- atom
	next <- peekMaybe
	case next of
		Just (Locate at (TOperator op)) -> if op `elem` ops then do
			advance 1 -- need to advance since we only 'peeked' next
			right <- parseInfix' ops atom -- the right side
			return $ OpBranch (OpAtom left) (Locate at op) right
			else
				return $ OpAtom left
		_ -> return $ OpAtom left

parseInfixLeft :: [String] -> Parse Expression -> Parse Expression
parseInfixLeft ops atom = fmap flattenLeft $ parseInfix' ops atom

parseInfixRight :: [String] -> Parse Expression -> Parse Expression
parseInfixRight ops atom = fmap flattenRight $ parseInfix' ops atom

locateTree :: OpTree -> Location
locateTree (OpAtom (Locate at _)) = at
locateTree (OpBranch left _ _) = locateTree left

flattenLeft :: OpTree -> Expression
flattenLeft (OpAtom atom) = atom
flattenLeft (OpBranch left op1 (OpBranch middle op2 right)) = flattenLeft $
	OpBranch (OpBranch left op1 middle) op2 right
flattenLeft (OpBranch left op right) = Locate (locateTree left)
	$ Operator (flattenLeft left) op (flattenLeft right)

flattenRight :: OpTree -> Expression
flattenRight (OpAtom atom) = atom
flattenRight (OpBranch (OpBranch left op1 middle) op2 right) = flattenRight $
	OpBranch left op1 (OpBranch middle op2 right)
flattenRight (OpBranch left op right) = Locate (locateTree left)
	$ Operator (flattenRight left) op (flattenRight right)
