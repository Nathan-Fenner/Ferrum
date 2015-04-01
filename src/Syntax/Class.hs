
module Syntax.Class where

import Location
import Syntax.Member
import Syntax.Kind

data Class = Class { className :: Name, classGeneric :: [Name], classKind :: Maybe Kind, classMembers :: [Member] } deriving Show
