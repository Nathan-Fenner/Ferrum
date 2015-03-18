
module Parse.Type where

import Parse.Core
import Lex
import Location

data TypeField = TypeField String [Type] deriving Show

type Type = Locate TypeField

parseType :: Parse Type
parseType = do
	n <- peekMaybe
	case n of
		Just (Locate nameAt (TWord name)) -> do
			advance 1 -- to skip the name
			open <- peekMaybe
			case open of
				Just (Locate openAt (TSpecial "[")) -> do
					advance 1 -- skip the "["
					first <- parseType
					rest <- manyWhile
						(do
							comma <- peekMaybe
							case comma of
								Just (Locate _commaAt (TSpecial ",")) -> do
									advance 1 -- skip the comma
									return True
								_ -> return False
						)
						parseType

					close <- peekMaybe
					case close of
						Just (Locate _closeAt (TSpecial "]")) -> do
							advance 1 -- skip it
							return $ Locate nameAt $ TypeField name (first : rest)
						_ -> crash $ Message $ "expected `]` to close `[` at " ++ displayLocation openAt
				_ -> return $ Locate nameAt $ TypeField name [] -- no args
		_ -> crash $ Message "Expected type name"