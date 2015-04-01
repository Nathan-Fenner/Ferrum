
module Syntax.Module where
import Location
import Syntax.Class

data Module = Module { modName :: Name, modClasses :: [Class]} deriving Show
