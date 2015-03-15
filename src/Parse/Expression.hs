
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

fix :: (x -> x) -> x
fix f = f (fix f)

parseProduct' :: Parse OpTree
parseProduct' = do
	left <- parseAtom
	next <- peekMaybe
	case next of
		Just (Locate at (TOperator op)) -> if op == "*" || op == "/" || op == "%" then do
			right <- parseProduct'
			return $ OpBranch (OpAtom left) (Locate at op) right
			else
				return $ OpAtom left
		_ -> return $ OpAtom left

parseProduct :: Parse Expression
parseProduct = fmap flattenLeft parseProduct'

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
