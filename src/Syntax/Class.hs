
module Syntax.Class where

import Location
import Syntax.Member

data Class = Class { className :: Name, classGeneric :: [Name], classMembers :: [Member] } deriving Show
