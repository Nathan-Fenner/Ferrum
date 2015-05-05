
module Verify.Module where

import Verify
import Syntax.Module
import Verify.Kind
import Verify.Type.Module
import Verify.Flow.Module

verifyModule :: Module -> Verify ()
verifyModule = verifyModuleKind >.> verifyModuleType >.> verifyModuleFlow