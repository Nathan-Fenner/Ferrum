
module Verify.Effect where

-- import Data.List(nub)
import Location

data Effects = Effects [EffectRecord] deriving Show

data EffectRecord
	= ModifyRecord Reference
	| AlterRecord Reference
	| RetainRecord Reference Reference
	| Discard Reference
	deriving (Show, Eq)

data Reference = Reference [Name] deriving (Show, Eq)


(=>=) :: Reference -> Reference -> Bool
Reference _ =>= Reference [] = True
Reference (x:xs) =>= Reference (y:ys)
	|value x == value y = Reference xs =>= Reference ys
_ =>= _ = False
