
module Parse.Expression where

import Message
import Parse.Core
import Parse.Type
import Lex
import Location
import Syntax.Expression

parseExpression :: Parse Expression
parseExpression = parseOperator

parseAtomCore :: Parse Expression
parseAtomCore = do
	Locate atExp token <- ask message
	case token of
		TSpecial "(" -> do
			expr <- parseExpression
			expect (TSpecial ")") $ Message $ "expected `)` to close `(` from " ++ displayLocation atExp
			return expr
		TWord name -> return $ Locate atExp $ Name name
		TInt int -> return $ Locate atExp $ LiteralInt int
		TString string -> return $ Locate atExp $ LiteralString string
		TSpecial "new" -> do
			conType <- parseType
			return $ Locate atExp $ New conType
		_ -> advance (-1) >> crash message
	where
		message = Message "expected name, literal, prefix operator or `(` to begin atomic expression"

parseAtom :: Parse Expression
parseAtom = do
	core <- parseAtomCore
	suffixes <- parseSuffixes
	return $ applySuffixes core suffixes

data Suffix = SuffixCall [Expression] | SuffixIndex Expression | SuffixDot Name deriving Show

parseSuffix :: Parse (Maybe (Locate Suffix))
parseSuffix = do
	whole <- peekMaybe
	case whole of
		Nothing -> return Nothing
		Just (Locate atSuffix token) -> case token of
			TSpecial "(" -> do -- it's a function call
				advance 1 -- to skip the `(`
				next <- peekMaybe
				case next of
					Nothing -> crash $ Message $ "expected `)` to close `(` that opened function call at " ++ displayLocation atSuffix
					Just (Locate _at' (TSpecial ")")) -> do
						advance 1
						return $ Just $ Locate atSuffix $ SuffixCall []
					Just (Locate _at' (TSpecial ",")) -> crash $ Message $ "expected `)` to close open `(` or expression before `,` at " ++ displayLocation atSuffix
					_ -> do
						-- there's at least one expression inside
						first <- parseExpression
						rest <- manyWhile (checkNext (TSpecial ","))
							(do
								advance 1 -- skip the `,`
								parseExpression
								)
						-- next we expect to find a `)`
						closing <- peekMaybe
						case closing of
							Just (Locate _at (TSpecial ")")) -> return ()
							_ -> crash $ Message $ "expected `)` to close opening `(` at " ++ displayLocation atSuffix
						advance 1 -- skip the `)`
						return $ Just $ Locate atSuffix $ SuffixCall $ first : rest
			TSpecial "[" -> do
				advance 1 -- skip the `[`
				index <- parseExpression
				expect (TSpecial "]") $ Message $ "expected `]` to close opening `[` at " ++ displayLocation atSuffix
				return $ Just $ Locate atSuffix $ SuffixIndex index
			TSpecial "." -> do
				advance 1 -- skip the `.`
				name <- expectName $ Message "expected name to follow `.`"
				return $ Just $ Locate atSuffix $ SuffixDot name
			_ -> return Nothing
parseSuffixes :: Parse [Locate Suffix]
parseSuffixes = manyMaybe parseSuffix

applySuffix :: Expression -> Locate Suffix -> Expression
applySuffix fun@(Locate funAt _) (Locate _at (SuffixCall call)) = Locate funAt $ Call fun call
applySuffix arg@(Locate argAt _) (Locate _at (SuffixIndex index)) = Locate argAt $ Index arg index
applySuffix expr@(Locate exprAt _) (Locate _at (SuffixDot name)) = Locate exprAt $ Dot expr name

applySuffixes :: Expression -> [Locate Suffix] -> Expression
applySuffixes e [] = e
applySuffixes e (s : ss) = applySuffixes (e `applySuffix` s) ss

data OpTree = OpAtom Expression | OpBranch OpTree Name OpTree

parsePrefix :: [String] -> Parse Expression -> Parse Expression
parsePrefix ops atom = do
	next <- peekMaybe
	case next of
		Just (Locate atPrefix (TOperator op)) -> if op `elem` ops then do
				advance 1 -- advance past this operator
				expr <- parsePrefix ops atom
				return $ Locate atPrefix $ Prefix (Locate atPrefix op) expr
			else
				atom
		_ -> atom

parseInfix' :: [String] -> Parse Expression -> Parse OpTree
parseInfix' ops atom = do
	left <- atom
	next <- peekMaybe
	case next of
		Just (Locate atInfix (TOperator op)) -> if op `elem` ops then do
			advance 1 -- need to advance since we only 'peeked' next
			right <- parseInfix' ops atom -- the right side
			return $ OpBranch (OpAtom left) (Locate atInfix op) right
			else
				return $ OpAtom left
		_ -> return $ OpAtom left

parseInfixLeft :: [String] -> Parse Expression -> Parse Expression
parseInfixLeft ops atom = fmap flattenLeft $ parseInfix' ops atom

parseInfixRight :: [String] -> Parse Expression -> Parse Expression
parseInfixRight ops atom = fmap flattenRight $ parseInfix' ops atom

locateTree :: OpTree -> Location
locateTree (OpAtom (Locate atTree _)) = atTree
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