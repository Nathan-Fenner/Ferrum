
module Syntax.Kind where

data Kind = Concrete | Arrow Kind Kind
	deriving (Show, Eq)

simpleKind :: Int -> Kind
simpleKind 0 = Concrete
simpleKind n
	|n < 0 = error "Arity for simple kind must be non-negative"
	|otherwise = Arrow Concrete $ simpleKind (n-1)

kindArity :: Kind -> Int
kindArity Concrete = 0
kindArity (Arrow _ right) = 1 + kindArity right
