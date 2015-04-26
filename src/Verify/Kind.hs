
module Verify.Kind where

import Verify
import Verify.Kind.Statement
import Verify.Kind.Class
import Verify.Kind.Type
import Syntax.Kind
import Syntax.Member
import Syntax.Class
import Syntax.Module
import Location



simpleModuleKindCheck :: Module -> Verify ()
simpleModuleKindCheck (Module _ classes) = mapM_ simpleClassKindCheck classes


verifyMemberKind :: [(String, Kind)] -> Member -> Verify ()
verifyMemberKind known member = case memberValue member of
	Field { fieldType = declared } -> verifyTypeConcrete known declared
	Method { methodReturnType = returnType, methodArguments = arguments, methodBody = body } -> do
		verifyTypeConcrete known returnType
		mapM_ (verifyTypeConcrete known . fst) arguments
		verifyBlockKind known body
	Constructor { constructorArguments = arguments, constructorBody = body } -> do
		mapM_ (verifyTypeConcrete known . fst) arguments
		verifyBlockKind known body

verifyClassKind :: [(String, Kind)] -> Class -> Verify ()
verifyClassKind known c@(Class { classMembers = members, classGeneric = generics }) = do
	let genericKinds = zipWith (,) (map value generics) (fst $ uncurryKind $ kindOfClass c)
	let totalList = genericKinds ++ known
	mapM_ (verifyMemberKind totalList) members

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
