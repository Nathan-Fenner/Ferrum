
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
		|value n `elem` map value ns = let chosen = filter (\c -> value c == value n) (n:ns) in
			Left $ Locate (at n) $ Message $
			"The class name `" ++ value n ++ "` is repeated at " ++ nice (map (displayLocation . at) chosen)
		|otherwise = unique ns
		where
		nice [] = ""
		nice [a] = a
		nice [a,b] = a ++ " and " ++ b
		-- we achieve the Oxford comma this way
		nice [a,b,c] = a ++ ", " ++ b ++ ", and " ++ c
		nice (x:xs) = x ++ ", " ++ nice xs

verifyModuleClasses :: Module -> Verify ()
verifyModuleClasses (Module _name classes) = mapM_ verifyClass classes

verifyModule :: Module -> Verify ()
verifyModule
	= verifyModuleClassesUnique
	>.> verifyModuleClasses