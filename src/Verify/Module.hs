
module Verify.Module where

import Verify
import Verify.Class
import Parse.Module
import Parse.Class
import Location
import Parse.Core

verifyModuleClassesUnique :: Module -> Verify ()
verifyModuleClassesUnique (Module _name classes) = unique $ map className classes where
	unique [] = return ()
	unique (n : ns)
		|value n `elem` map value ns = Left $ Locate (at n) $ Message $ "Class name `" ++ value n ++ "` is not unique"
		|otherwise = unique ns

verifyModuleClasses :: Module -> Verify ()
verifyModuleClasses (Module _name classes) = mapM_ verifyClass classes

verifyModule :: Module -> Verify ()
verifyModule
	= verifyModuleClassesUnique
	>.> verifyModuleClasses