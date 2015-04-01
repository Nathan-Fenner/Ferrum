
module Verify.Type.Form where

import Message
import Parse.Type
import Verify
import Location


data Form
	= FormOf Type
	| Arrow Form Form
	| Concrete Location
	deriving Show

data Equiv = Equiv Type Form
	deriving Show

generate :: Type -> [Equiv]
generate field
	|null args = []
	|otherwise = (Equiv (Type (typeName field) []) $ arrowBuild args) : concat (map generate args)
	where
	args :: [Type]
	args = typeArguments field
	arrowBuild :: [Type] -> Form
	arrowBuild [] = FormOf field
	arrowBuild (x:xs) = Arrow (FormOf x) $ arrowBuild xs

generateIs :: Type -> Type -> [Equiv]
generateIs thing is = Equiv thing (FormOf is) : generate thing

formOfEqual :: Form -> Form -> Bool
formOfEqual (FormOf a) (FormOf b) = go a b where
	go :: Type -> Type -> Bool
	go x y
		|(value $ typeName x) /= (value $ typeName y) = False
		|length xa /= length ya = False
		|otherwise = and $ zipWith go xa ya
		where
		xa = typeArguments x
		ya = typeArguments y
formOfEqual _ _ = False

formContains :: Type -> Form -> Bool
formContains f x
	|formOfEqual (FormOf f) x = True
	|otherwise = case x of
		Arrow left right -> formContains f left || formContains f right
		_ -> False

obtainFrom :: Type -> [Equiv] -> Maybe Form
obtainFrom _ [] = Nothing
obtainFrom e (Equiv l r:xs)
	|formOfEqual (FormOf l) (FormOf e) = Just r
	|otherwise = obtainFrom e xs

unifyForm :: [Equiv] -> Form -> Form -> Verify [Equiv]
unifyForm known (FormOf name) right = unify known (Equiv name right)
unifyForm known (Concrete _) (Concrete _) = return known
unifyForm known (Arrow leftA rightA) (Arrow leftB rightB) = do
	known' <- unifyForm known leftA leftB
	unifyForm known' rightA rightB
unifyForm _known (Concrete loc) (Arrow _ _) = Left $ Locate loc $ Message $ "attempted to unify `*` with `_ -> _`"
unifyForm _known (Arrow _ _) (Concrete loc) = Left $ Locate loc $ Message $ "attempted to unify `_ -> _` with `*`"
unifyForm known left (FormOf right) = unifyForm known (FormOf right) left

unify :: [Equiv] -> Equiv -> Verify [Equiv]
unify known (Equiv left (FormOf right))
	|formOfEqual (FormOf left) (FormOf right) = return known -- TODO: 'fail for infinite type'
	|otherwise = case obtainFrom left known of
		Nothing -> case obtainFrom right known of
			Nothing -> return $ (Equiv left (FormOf right)) : known
			Just right' -> unify known (Equiv left right')
		Just left' -> unify known (Equiv right left')
unify known (Equiv left right) = case obtainFrom left known of
	Nothing -> return $ Equiv left right : known
	Just left' -> unifyForm known left' right 

unifyAll :: [Equiv] -> Verify [Equiv]
unifyAll xs = go [] xs where
	go :: [Equiv] -> [Equiv] -> Verify [Equiv]
	go old [] = return old
	go old (next:rest) = do
		new <- unify old next
		go new rest

unifyWith' :: [Equiv] -> [Equiv] -> Verify [Equiv]
unifyWith' left right = unifyAll $ left ++ right

unifyWith :: [Equiv] -> Verify [Equiv] -> Verify [Equiv]
unifyWith xs ys = ys >>= unifyWith' xs
