
module Verify.Kind where

import Verify
import Syntax.Kind
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

