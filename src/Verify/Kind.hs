
module Verify.Kind where

import Verify
import Syntax.Kind
import Syntax.Type
import Syntax.Class
import Syntax.Module
import Location
import Message

kindOfClass :: Class -> Kind
kindOfClass c = case classKind c of
	Nothing -> simpleKind (length $ classGeneric c)
	Just k -> k

simpleClassKindCheck :: Class -> Verify ()
simpleClassKindCheck c = case
	kindArity (kindOfClass c) == length (classGeneric c) of
		True -> return ()
		False -> Left $ Locate (at $ className c) $ Message $ "class `" ++ value (className c) ++ "` has incompatible kind `(" ++ niceKind (kindOfClass c) ++ ")` with its number of formal parameters (" ++ (show $ length $ classGeneric c) ++ ") which would predict a kind of the form `(" ++ (exampleKind $ length $ classGeneric c) ++ ")`"

simpleModuleKindCheck :: Module -> Verify ()
simpleModuleKindCheck (Module _ classes) = mapM_ simpleClassKindCheck classes

kindFrom :: [(String, Kind)] -> Name -> Verify Kind
kindFrom [] name = Left $ Locate (at name) $ Message $ "cannot find kind information (could be out-of-scope or misspelled) for type `" ++ value name ++ "`"
kindFrom ((v, k) : t) name
	|v == value name = return k
	|otherwise = kindFrom t name

assert :: Bool -> Locate Message -> Verify ()
assert v msg
	|not v = Left msg
	|otherwise = return ()

kindOfType :: [(String, Kind)] -> Type -> Verify Kind
kindOfType known (Type n []) = do
	kindFrom known n
kindOfType known (Type n args) = do
	initKind <- kindOfType known (Type n (init args))
	case initKind of
		Concrete -> Left $ Locate (typeAt $ last args) $ Message $ "Type `" ++ value n ++ "` at " ++ displayLocation (at n) ++ " is applied to too many arguments"
		Arrow left right -> do
			argKind <- kindOfType known (last args)
			assert (left == argKind) $ Locate (typeAt $ last args) $ Message $ "Type argument `" ++ show (last args) ++ "` to type `" ++ (value n) ++ "` at " ++ displayLocation (at n) ++ " has incorrect kind; expected " ++ niceKind left ++ " but got " ++ niceKind argKind
			return right
