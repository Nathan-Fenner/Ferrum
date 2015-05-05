
module Verify.Flow.Member where

import Verify
import Verify.Flow.Statement
import Syntax.Type
import Syntax.Member

memberFlowVerify :: Member -> Verify ()
memberFlowVerify Member { memberValue = Method { methodReturnType = returnType, methodBody = body } } = methodBodyFlowVerify body (returnType == voidType)
memberFlowVerify _ = return ()
