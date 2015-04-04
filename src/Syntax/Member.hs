
module Syntax.Member where
import Location
import Syntax.Modifier
import Syntax.Type
import Syntax.Expression
import Syntax.Statement
import Syntax.Effect

data Visibility
	= Public
	| Private
	| Protected
	deriving Show

data Member = Member {memberVisibility :: Visibility, memberValue :: MemberValue}
	deriving Show

data MemberValue
	= Field
		{ fieldModifier :: Modifier
		, fieldType :: Type
		, fieldName :: Name
		}
	| Method
		{ methodReturnType :: Type
		, methodFromExpression :: (Maybe Expression)
		, methodName :: Name
		, methodArguments :: [(Type, Name)]
		, methodEffects :: [Effect]
		, methodBody :: [Statement]
		}
	| Constructor
		{ startLocation :: Location
		, constructorArguments :: [(Type, Name)]
		, constructorEffects :: [Effect]
		, constructorBody :: [Statement]
		}
	deriving Show
