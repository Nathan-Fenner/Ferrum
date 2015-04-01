
module Verify.Class where

import Message
import Syntax.Class
import Verify
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
verifyClassGenericArgumentsHash given = mapM_ inspect $ classGeneric given where
	inspect :: Name -> Verify ()
	inspect name
		|null (value name) = Left $ Locate (at name) $ Message $ "generic argument name cannot be empty"
		|head (value name) == '#' = return ()
		|otherwise = Left $ Locate (at name) $ Message $ "generic argument `" ++ value name ++ "` must begin with `#` but does not"


verifyClassGenericArgumentsUnique :: Class -> Verify ()
verifyClassGenericArgumentsUnique given = unique $ classGeneric given where
	unique :: [Name] -> Verify ()
	unique [] = return ()
	unique (n : ns)
		|value n `elem` map value ns = Left $ Locate (at n) $ Message $ "generic argument `" ++ (value n) ++ "` is not unique"
		|otherwise = unique ns

verifyClass :: Class -> Verify ()
verifyClass
	= verifyClassName
	>.> verifyClassGenericArgumentsHash
	>.> verifyClassGenericArgumentsUnique
	>.> const (return ())

