
module Verify.Kind where

import Verify
import Verify.Kind.Class
import Syntax.Kind
import Syntax.Class
import Syntax.Module
import Location



simpleModuleKindCheck :: Module -> Verify ()
simpleModuleKindCheck (Module _ classes) = mapM_ simpleClassKindCheck classes




verifyModuleKind :: Module -> Verify ()
verifyModuleKind m@Module{ modClasses = classes } = do
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
