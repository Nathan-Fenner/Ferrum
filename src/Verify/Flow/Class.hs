
module Verify.Flow.Class where

import Syntax.Class
import Verify
import Verify.Flow.Member

verifyFlowClass :: Class -> Verify ()
verifyFlowClass Class { classMembers = members } = mapM_ memberFlowVerify members
