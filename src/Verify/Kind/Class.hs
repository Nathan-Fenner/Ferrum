
module Verify.Kind.Class where

import Message
import Syntax.Kind
import Syntax.Class
import Syntax.Member
import Syntax.Type
import Verify
import Verify.Kind.Member
import Location

kindOfClass :: Class -> Kind
kindOfClass c = case classKind c of
	Nothing -> simpleKind (length $ classGeneric c)
	Just k -> k

simpleClassKindCheck :: Class -> Verify ()
simpleClassKindCheck c = assert (kindArity (kindOfClass c) == length (classGeneric c)) $ Locate (at $ className c) $ Message $ "class `" ++ value (className c) ++ "` has incompatible kind-arity `(" ++ niceKind (kindOfClass c) ++ ")` with its number of formal parameters (" ++ (show $ length $ classGeneric c) ++ ") which would predict a kind of the form `(" ++ (exampleKind $ length $ classGeneric c) ++ ")`"

verifyClassKind :: [(String, Kind)] -> Class -> Verify ()
verifyClassKind known c@(Class { classMembers = members, classGeneric = generics }) = do
	let genericKinds = zipWith (,) (map value generics) (fst $ uncurryKind $ kindOfClass c)
	let totalList = genericKinds ++ known
	mapM_ (verifyMemberKind totalList) members

-------------------

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

verifyMethodsUnique :: Class -> Verify ()
verifyMethodsUnique c = mapM_ testPairMethod methodPairs >> mapM_ testPairConstructor constructorPairs where
	uniquePairs :: [a] -> [(a,a)]
	uniquePairs [] = []
	uniquePairs (x:xs) = map ((,) x) xs ++ uniquePairs xs
	methodPairs :: [((Name, [(Type, Name)]),(Name, [(Type, Name)]))]
	methodPairs = uniquePairs $ onlyMethods $ classMembers c
	onlyMethods :: [Member] -> [(Name, [(Type, Name)])]
	onlyMethods [] = []
	onlyMethods (m : ms) = case memberValue m of
		Method { methodName = name, methodArguments = arguments } -> (name, arguments) : onlyMethods ms
		_ -> onlyMethods ms
	onlyConstructors :: [Member] -> [(Location,[(Type, Name)])]
	onlyConstructors [] = []
	onlyConstructors (con : cs) = case memberValue con of
		Constructor { startLocation = location, constructorArguments = arguments } -> (location, arguments) : onlyConstructors cs
		_ -> onlyConstructors cs
	constructorPairs :: [((Location, [(Type, Name)]), (Location, [(Type, Name)]))]
	constructorPairs = uniquePairs $ onlyConstructors $ classMembers c

	testPairConstructor ((locA, argsA), (locB, argsB)) = case unifyMethodArguments argsA argsB of
		Nothing -> return ()
		Just values -> Left $ Locate locB $ Message $
			"illegal class definition. constructors are ambiguous, from definitions at " ++ displayLocation locA ++ " and " ++ displayLocation locB ++ ". " ++ (concatKeys $ map (\(v, t) -> "\n" ++ v ++ " = " ++ prettyType t) values)


	testPairMethod ((nameA, argsA),(nameB, argsB))
		|value nameA /= value nameB = return ()
		|otherwise = case unifyMethodArguments argsA argsB of
			Nothing -> return ()
			Just values -> Left $ Locate (at nameB) $ Message $
				"illegal class definition. method `" ++ value nameA ++ "` is ambiguous, from definitions at " ++ displayLocation (at nameA) ++ " and " ++ displayLocation (at nameB) ++ ". " ++ (concatKeys $  map (\(v,t) -> "\n" ++ v ++ " = " ++ prettyType t) values)
	
	concatKeys [] = "Ambiguous case occurs from identical argument signature"
	concatKeys x = "Ambiguous case occurs when " ++ concat x


verifyClass :: Class -> Verify ()
verifyClass
	= verifyClassName
	>.> verifyClassGenericArgumentsHash
	>.> verifyClassGenericArgumentsUnique
	>.> verifyMethodsUnique
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
	|otherwise = Nothing -- can't possibly match, since some additional structure exists in 'n'
	where
	a = value a'

unifyTypes :: [(String, Type)] -> Type -> Type -> Maybe [(String, Type)]
unifyTypes unifies a b
	|atomic a && atomic b = unifyAtomicTypes unifies (typeName a) (typeName b)
	|atomic a && not (atomic b) = unifyAtomicNonatomicTypes unifies (typeName a) b
	|not (atomic a) && atomic b = unifyAtomicNonatomicTypes unifies (typeName b) a
	|otherwise = unifyTypes unifies lastA lastB >>= (\us -> unifyTypes us shortA shortB)
	where
	lastA = last $ typeArguments a
	lastB = last $ typeArguments b
	shortA = Type (typeName a) (init $ typeArguments a)
	shortB = Type (typeName b) (init $ typeArguments b)

unifyTogether :: [(String, Type)] -> [(String, Type)] -> Maybe [(String, Type)]
unifyTogether left [] = Just left
unifyTogether left (first@(firstName, firstType) : right) = case fromUnifies left firstName of
	Nothing -> unifyTogether (first : left) right
	Just thing -> unifyTypes left thing firstType >>= flip unifyTogether right

unifyMany :: [[(String, Type)]] -> Maybe [(String, Type)]
unifyMany list = go [] list where
	go x [] = Just x
	go x (y:ys) = do
		r <- unifyTogether x y
		go r ys

unifyMethodArguments :: [(Type, Name)] -> [(Type, Name)] -> Maybe [(String, Type)]
unifyMethodArguments left right
	| length left /= length right = Nothing
	| otherwise = sequence (zipWith (unifyTypes []) (map fst left) (map fst right)) >>= unifyMany
