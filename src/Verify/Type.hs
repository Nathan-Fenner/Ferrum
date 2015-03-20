
module Verify.Type where
import Parse.Core
import Parse.Type
import Verify
import Location

verifyTypeForm :: Type -> Verify ()
verifyTypeForm typeField = formed (value $ typeName $ value typeField) >> mapM_ verifyTypeForm (typeArguments $ value typeField) where
	formed "" = Left $ Locate (at typeField) $ Message $ "type names must not be empty, at " ++ displayLocation (at typeField)
	formed name@(c:_)
		|c `elem` ['A'..'Z'] ++ "#" = return ()
		|otherwise = Left $ Locate (at typeField) $ Message $ "type `" ++ name ++ "` begins with non-capital (and not `#`) letter `" ++ [c] ++ "`"
