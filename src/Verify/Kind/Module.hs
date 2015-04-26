
module Verify.Kind.Module where

import Message
import Verify
import Verify.Kind.Class
import Syntax.Module
import Syntax.Class
import Syntax.Kind
import Location

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

simpleModuleKindCheck :: Module -> Verify ()
simpleModuleKindCheck (Module _ classes) = mapM_ simpleClassKindCheck classes




verifyModuleKindEach :: Module -> Verify ()
verifyModuleKindEach m@Module{ modClasses = classes } = do
	-- first, check that everything in the module has an appropriate kind
	-- in particular, that their generic arguments match their explicit kind
	-- if this explicit kind is stated
	simpleModuleKindCheck m
	-- now we can assume that they're correct and consistent
	let internal = map (\c -> (value $ className c, kindOfClass c)) classes

	-- TODO: actually get the REAL external (requires loading all imported modules)
	let external = [("Void",Concrete),("Int",Concrete),("Maybe",Arrow Concrete Concrete)]
	
	let known = internal ++ external
	-- internals must be checked too
	mapM_ (verifyClassKind known) classes
	return ()

