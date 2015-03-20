
module Parse.Module where
import Parse.Class
import Parse.Core
import Lex
import Location

data Module = Module Name [Class] deriving Show

parseModule = do
	expect (TSpecial "module") $ Message $ "expected module to begin with `module` keyword"
	Locate nameAt name <- expectName $ Message $ "expected module name to follow `module` keyword"
	expect (TSpecial "{") $ Message $ "expected `{` to follow module name in module `" ++ name ++ "`"
	members <- manyUntil (checkNext (TSpecial "}")) parseClass
	expect (TSpecial "}") $ Message $ "expected `}` to end module `" ++ name ++ "`"
	return $ Module (Locate nameAt name) members