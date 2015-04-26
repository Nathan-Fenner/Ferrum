
module Verify.Kind where

import Verify
import Verify.Kind.Module
import Syntax.Module



verifyModuleKind :: Module -> Verify ()
verifyModuleKind
	= verifyModuleClassesUnique
	>.> verifyModuleClasses
	>.> verifyModuleKindEach
	
