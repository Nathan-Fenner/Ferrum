
module Parse.Expression where

import Parse.Core
import Lex
import Location

type Expression = Locate ExpressionValue

data ExpressionValue
	= Name String
	| LiteralInt Int
	| LiteralString String
	| Operator Expression Name Expression
	| Prefix Name Expression
	| Call Expression [Expression]
	| Index Expression Expression
	| Dot Expression Name
	deriving Show

parseExpression :: Parse Expression
parseExpression = parseOperator

parseAtom :: Parse Expression
parseAtom = do
	Locate at token <- ask message
	case token of
		TSpecial "(" -> do
			expr <- parseExpression
			Locate parenAt closeParen <- ask parenMessage
			case closeParen of
				TSpecial ")" -> return expr
				_ -> crash parenMessage
			where
			parenMessage = Message $ "expected `)` to close `(` from " ++ displayLocation at

		TWord name -> return $ Locate at $ Name name
		TInt int -> return $ Locate at $ LiteralInt int
		TString string -> return $ Locate at $ LiteralString string
		_ -> crash message
	where
		message = Message "expected literal"

data Suffix = SuffixCall [Expression] | SuffixIndex Expression | SuffixDot Name deriving Show

parseSuffix :: Parse (Maybe (Locate Suffix))
parseSuffix = do
	whole <- peekMaybe
	case whole of
		Nothing -> return Nothing
		Just (Locate at token) -> case token of
			TSpecial "(" -> do -- it's a function call
				advance 1 -- to skip the `(`
				next <- peekMaybe
				case next of
					Nothing -> crash $ Message $ "expected `)` to close `(` that opened function call at " ++ displayLocation at
					Just (Locate _at' (TSpecial ")")) -> do
						advance 1
						return $ Just $ Locate at $ SuffixCall []
					Just (Locate _at' (TSpecial ",")) -> crash $ Message $ "expected `)` to close open `(` or expression before `,` at " ++ displayLocation at
					_ -> do
						-- there's at least one expression inside
						first <- parseExpression
						rest <- manyWhile (do
								x <- peekMaybe
								case x of
									Just (Locate _ (TSpecial ",")) -> return True
									_ -> return False
							)
							(do
								advance 1 -- skip the `,`
								parseExpression
								)
						-- next we expect to find a `)`
						closing <- peekMaybe
						case closing of
							Just (Locate _at (TSpecial ")")) -> return ()
							_ -> crash $ Message $ "expected `)` to close opening `(` at " ++ displayLocation at
						advance 1 -- skip the `)`
						return $ Just $ Locate at $ SuffixCall $ first : rest
			TSpecial "[" -> do
				advance 1 -- skip the `[`
				index <- parseExpression
				next <- peekMaybe
				case next of
					Just (Locate _at (TSpecial "]")) -> do
						advance 1
						return $ Just $ Locate at $ SuffixIndex index
					_ -> crash $ Message $ "expected `]` to close opening `[` at " ++ displayLocation at
			TSpecial "." -> do
				advance 1 -- skip the `.`
				next <- peekMaybe
				case next of
					Just (Locate nameAt (TWord name)) -> do
						advance 1
						return $ Just $ Locate at $ SuffixDot $ Locate nameAt name
					_ -> crash $ Message $ "expected name to follow `.`"

parseSuffixes :: Parse [Locate Suffix]
parseSuffixes = manyMaybe parseSuffix

applySuffix :: Expression -> Locate Suffix -> Expression
applySuffix fun@(Locate funAt _) (Locate at (SuffixCall call)) = Locate funAt $ Call fun call
applySuffix arg@(Locate argAt _) (Locate at (SuffixIndex index)) = Locate argAt $ Index arg index
applySuffix expr@(Locate exprAt _) (Locate at (SuffixDot name)) = Locate exprAt $ Dot expr name

applySuffixes :: Expression -> [Locate Suffix] -> Expression
applySuffixes e [] = e
applySuffixes e (s : ss) = applySuffixes (e `applySuffix` s) ss

data OpTree = OpAtom Expression | OpBranch OpTree Name OpTree

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

parseOperators :: [Parse Expression -> Parse Expression] -> Parse Expression
parseOperators [] = parseAtom
parseOperators (fun : rest) = fun $ parseOperators rest

parseOperator :: Parse Expression
parseOperator = parseOperators
	[ parseInfixLeft ["or"]
	, parseInfixLeft ["and"]
	, parsePrefix ["not"]
	, parseInfixLeft ["==","~=",">=","<=",">","<"]
	, parseInfixRight ["++"]
	, parseInfixLeft ["+","-"]
	, parsePrefix ["+","-"]
	, parseInfixLeft ["*", "/", "%"]
	]