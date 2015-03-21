
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

type ArityFact = (String, Locate Int)



makeArity :: Name -> Int -> ArityFact
makeArity name arity = (value name, Locate (at name) arity)

arityFromFact :: ArityFact -> String -> Maybe (Locate Int)
arityFromFact fact name
	|fst fact == name = Just $ snd fact
	|otherwise = Nothing

arityFromFacts :: [ArityFact] -> String -> Maybe (Locate Int)
arityFromFacts facts name = case [val' | (name', val') <- facts, name == name'] of
	[] -> Nothing
	(val : _) -> Just $ val

addArity :: [ArityFact] -> ArityFact -> Verify [ArityFact]
addArity facts (name, Locate newAt newArity) = case arityFromFacts facts name of
	Nothing -> return $ (name, Locate newAt newArity) : facts
	Just (Locate oldAt oldArity) -> if newArity == oldArity
		then
			return facts
		else
			Left $ Locate newAt $ Message $ "arity of `" ++ name ++ "` (" ++ show newArity ++ ") is inconsistent with its arity (" ++ show oldArity ++ ") at " ++ displayLocation oldAt

unifyArities :: [ArityFact] -> [ArityFact] -> Verify [ArityFact]
unifyArities [] ys = return ys
unifyArities (x : xs) ys = addArity ys x >>= unifyArities xs
