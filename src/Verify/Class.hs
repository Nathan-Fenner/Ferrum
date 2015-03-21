
module Verify.Class where

import Parse.Core
import Parse.Type
import Parse.Class
import Verify
import Verify.Type
import Verify.Member
import Location

verifyClassName :: Class -> Verify ()
verifyClassName given = inspect (value name) where
	inspect "" = Left $ Locate (at name) $ Message $ "class name is empty" -- theoretically, this shouldn't happen
	inspect (c : _)
		|c `elem` ['A'..'Z'] = return () -- totally fine
		|otherwise = Left $ Locate (at name) $
			Message $
				"type names must begin with an uppercase letter: `" ++ value name ++ "`"
	name = className given

verifyClassGenericArgumentsHash :: Class -> Verify ()
verifyClassGenericArgumentsHash given = mapM_ (inspect . typeName . value) $ classGeneric given where
	inspect :: Name -> Verify ()
	inspect name
		|null (value name) = Left $ Locate (at name) $ Message $ "generic argument name cannot be empty"
		|head (value name) == '#' = return ()
		|otherwise = Left $ Locate (at name) $ Message $ "generic argument `" ++ value name ++ "` must begin with `#` but does not"


verifyClassGenericArgumentsUnique :: Class -> Verify ()
verifyClassGenericArgumentsUnique given = unique $ map (typeName . value) $ classGeneric given where
	unique :: [Name] -> Verify ()
	unique [] = return ()
	unique (n : ns)
		|value n `elem` map value ns = Left $ Locate (at n) $ Message $ "generic argument `" ++ (value n) ++ "` is not unique"
		|otherwise = unique ns

verifyClassGenericArgumentsZeroArity :: Class -> Verify ()
verifyClassGenericArgumentsZeroArity given = mapM_ inspect (classGeneric given) where
	inspect :: Type -> Verify ()
	inspect typeField
		|null (typeArguments $ value typeField) = return ()
		|otherwise = Left $ Locate (at typeField) $ Message $ "generic argument names may not have parameters / generic argument type `" ++ (value . typeName . value $ typeField) ++ "` has been given generic arguments"

verifyClassGenericArgumentsForm :: Class -> Verify ()
verifyClassGenericArgumentsForm given = mapM_ verifyTypeForm (classGeneric given)

verifyClass :: Class -> Verify ()
verifyClass
	= verifyClassName
	>.> verifyClassGenericArgumentsHash
	>.> verifyClassGenericArgumentsUnique
	>.> verifyClassGenericArgumentsZeroArity
	>.> verifyClassGenericArgumentsForm
	>.> verifyClassTypeArity
	>.> const (return ())

verifyClassTypeArity :: Class -> Verify [ArityFact]
verifyClassTypeArity given = unifyVerifyArity $ map verifyMemberGenericArity $ classMembers given