
module Verify.Flow.Module where

import Syntax.Module
import Verify
import Verify.Flow.Class

verifyModuleFlow :: Module -> Verify ()
verifyModuleFlow Module { modClasses = classes } = mapM_ verifyFlowClass classes
