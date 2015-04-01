
module Parse.Modifier where

import Syntax.Modifier
import Parse.Core
import Lex
import Location

parseModifier :: Parse Modifier
parseModifier = do
	next <- peekMaybe
	case next of
		Just (Locate _ (TSpecial "mut")) -> advance 1 >> return Mutable
		Just (Locate _ (TSpecial "alt")) -> advance 1 >> return Alterable
		Just (Locate _ (TSpecial "fin")) -> advance 1 >> return Final
		_ -> return None
