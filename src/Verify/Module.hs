
module Verify.Module where

import Verify
import Syntax.Module
import Verify.Kind
import Verify.Type.Module

verifyModule :: Module -> Verify ()
verifyModule = verifyModuleKind >.> verifyModuleType