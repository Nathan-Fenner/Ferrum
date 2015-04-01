
module Parse.Kind where

import Syntax.Kind
import Parse.Core
import Message
import Location
import Lex

parseKindAtom :: Parse Kind
parseKindAtom = do
	Locate a t <- ask $ Message $ "Expected kind, in particular a `#` or a `(`"
	case t of
		TSpecial "(" -> do
			k <- parseKind
			expect (TSpecial ")") $ Message $ "Expected `)` to match `(` in kind at " ++ displayLocation a
			return k
		TSpecial "#" -> do
			return Concrete
		_ -> advance (-1) >> (crash $ Message $ "Expected kind, in particular a `#` or a `(`")

parseKind :: Parse Kind
parseKind = do
	k <- parseKindAtom
	ks <- manyWhile
		(do
		p <- peekMaybe
		case p of
			Just (Locate _ (TOperator "->")) -> return True
			_ -> return False
		)
		(do
		expect (TOperator "->") $ Message $ "must have been a `->` here as part of kind"
		parseKindAtom
		)
	return $ foldup k ks
	where
	foldup k [] = k
	foldup k' (k:ks) = Arrow k' (foldup k ks)

parseKindLabel :: Parse (Maybe Kind)
parseKindLabel = do
	label <- peekMaybe
	case label of
		Just (Locate _ (TOperator "::")) -> do
			expect (TOperator "::") $ Message $ "must have been a `::` here preceding kind definition"
			fmap Just parseKind
		_ -> return Nothing


