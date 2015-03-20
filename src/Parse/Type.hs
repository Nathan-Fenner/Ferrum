
module Parse.Type where

import Parse.Core
import Lex
import Location

data TypeField = TypeField { typeName :: Name, typeArguments :: [Type] } deriving Show

type Type = Locate TypeField

parseType :: Parse Type
parseType = do
	Locate nameAt name <- expectName $ Message "expected type name"
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
			expect (TSpecial "]") (Message $ "expected `]` to close `[` at " ++ displayLocation openAt)
			return $ Locate nameAt $ TypeField (Locate nameAt name) (first : rest)
			
		_ -> return $ Locate nameAt $ TypeField (Locate nameAt name) [] -- no args