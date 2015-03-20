
module Verify.Class where

import Parse.Core
import Parse.Class
import Location

verifyClassName :: Class -> Verify ()
verifyClassName given = inspect (value name) where
	inspect "" = Left $ Message $ "class name is empty" -- theoretically, this shouldn't happen
	inspect (c : _)
		|c `elem` ['A'..'Z'] = return () -- totally fine
		|otherwise = Left $
			Message $
				"type names must begin with an uppercase letter: `" ++ value name ++ "` at " ++ displayLocation (at name)
	name = className given

