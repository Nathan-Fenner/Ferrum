
module Verify.Type.Form where

import Parse.Type
import Parse.Core
import Verify
import Location


data Form
	= FormOf TypeField
	| Arrow Form Form
	| Concrete
	deriving Show

data Equiv = Equiv TypeField Form
	deriving Show

generate :: Type -> [Equiv]
generate field'
	|null args = []
	|otherwise = (Equiv (TypeField (typeName field) []) $ arrowBuild args) : concat (map generate args)
	where
	field = value field'
	args = typeArguments field
	arrowBuild [] = FormOf field
	arrowBuild (x:xs) = Arrow (FormOf $ value x) $ arrowBuild xs

generateIs :: Type -> Type -> [Equiv]
generateIs thing is = Equiv (value thing) (FormOf $ value is) : generate thing

formOfEqual :: Form -> Form -> Bool
formOfEqual (FormOf a) (FormOf b) = go a b where
	go x y
		|value (typeName x) /= value (typeName y) = False
		|length xa /= length ya = False
		|otherwise = and $ zipWith go (map value xa) (map value ya)
		where
		xa = typeArguments x
		ya = typeArguments y
formOfEqual _ _ = False

formContains :: TypeField -> Form -> Bool
formContains f x
	|formOfEqual (FormOf f) x = True
	|otherwise = case x of
		Arrow left right -> formContains f left || formContains f right
		_ -> False

obtainFrom :: TypeField -> [Equiv] -> Maybe Form
obtainFrom _ [] = Nothing
obtainFrom e (Equiv l r:xs)
	|formOfEqual (FormOf l) (FormOf e) = Just r
	|otherwise = obtainFrom e xs

unifyForm :: [Equiv] -> Form -> Form -> Verify [Equiv]
unifyForm known (FormOf name) right = unify known (Equiv name right)
unifyForm known Concrete Concrete = return known
unifyForm known (Arrow leftA rightA) (Arrow leftB rightB) = do
	known' <- unifyForm known leftA leftB
	unifyForm known' rightA rightB
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