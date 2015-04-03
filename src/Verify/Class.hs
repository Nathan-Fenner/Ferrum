
module Verify.Class where

import Message
import Syntax.Class
import Syntax.Type
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


fromUnifies :: [(String, Type)] -> String -> Maybe Type
fromUnifies unifies name = case map snd $ filter (\(n,_) -> n == name) unifies of
	[] -> Nothing
	(x:_) -> Just x

hasUnify :: [(String, Type)] -> String -> Bool
hasUnify list name = name `elem` map fst list

atomic :: Type -> Bool
atomic (Type _ []) = True
atomic _ = False

unifyAtomicTypes :: [(String, Type)] -> Name -> Name -> Maybe [(String, Type)]
unifyAtomicTypes unifies a' b'
	|a == b = Just unifies
	|null a || null b = error "TYPES ARE EMPTY?"
	|head a == '#' && head b == '#' = case fromUnifies unifies a of
		Nothing -> case fromUnifies unifies b of
			Nothing -> Just $ (a, tB) : unifies
			Just otherB -> unifyTypes unifies tA otherB
		Just otherA -> unifyTypes unifies otherA tB
	|head a == '#' = case fromUnifies unifies a of
		Nothing -> Just $ (a, tB) : unifies
		Just otherA -> unifyTypes unifies otherA tB
	|head b == '#' = case fromUnifies unifies b of
		Nothing -> Just $ (b, tA) : unifies
		Just otherB -> unifyTypes unifies tA otherB
	|otherwise = if a == b then Just unifies else Nothing
	where
	a = value a'
	b = value b'
	tA = Type a' []
	tB = Type b' []

unifyAtomicNonatomicTypes :: [(String, Type)] -> Name -> Type -> Maybe [(String, Type)]
unifyAtomicNonatomicTypes unifies a' n
	|null a = error "TYPE IS EMPTY?"
	|head a == '#' = case fromUnifies unifies a of
		Nothing -> Just $ (a, n) : unifies
		Just other -> unifyTypes unifies other n
	where
	a = value a'


unifyTypes :: [(String, Type)] -> Type -> Type -> Maybe [(String, Type)]
unifyTypes unifies a b
	|atomic a && atomic b = unifyAtomicTypes unifies (typeName a) (typeName b)