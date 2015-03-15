
module Parse.Core where

import Location
import Control.Applicative



newtype Parse token result = Parse { run :: [token] -> (Int, Either Message result ) }

instance Functor (Parse token) where
	fmap f (Parse g) = Parse $ \i -> let (a, r) = g i in case r of
		Left msg -> (a, Left msg)
		Right result -> (a, Right $ f result)

instance Applicative (Parse token) where
	pure x = Parse $ \i -> (0, Right x)
	
	Parse f <*> Parse g = Parse $ \i -> let (af, rf) = f i in case rf of
		Left msg -> (af, Left msg)
		Right resultFun -> let (ag, rg) = g (drop af i) in case rg of
			Left msg' -> (af + ag, Left msg')
			Right resultArg -> (af + ag, Right $ resultFun resultArg)
		where


instance Monad (Parse token) where
	return x = pure x
	Parse f >>= into = Parse $ \i -> let (af, rf) = f i in case rf of
		Left msg -> (af, Left msg)
		Right thing -> let Parse q = into thing; (aq, rq) = q (drop af i) in (aq + af, rq)

newtype Message = Message String deriving Show

advance :: Int -> Parse x ()
advance n = Parse $ const (n, Right ())

peekLocation :: [Locate token] -> Location
peekLocation [] = End
peekLocation (Locate {at = loc} : _) = loc


expectString :: String -> Message -> Parse (Locate String) ()
expectString string message = Parse $ \i -> case i of
	[] -> (0, Left message)
	(Locate m t : _) -> if t == string then
		(1, Right ())
		else (0, Left message)

peekMaybe :: Parse token (Maybe token)
peekMaybe = Parse $ \i -> case i of
	[] -> (0, Right Nothing)
	(token : _) -> (0, Right $ Just token)

peek :: Message -> Parse (Locate token) (Locate token)
peek msg = Parse $ \i -> case i of
	[] -> (0, Left msg)
	(token : _) -> (0, Right $ token)

askMaybe :: Parse token (Maybe token)
askMaybe = do
	token <- peekMaybe
	advance 1
	return token

ask :: Message -> Parse (Locate token) (Locate token)
ask msg = do
	token <- peek msg
	advance 1
	return token

test :: Parse token Bool -> Parse token x -> Parse token x -> Parse token x
test condition first other = do
	c <- condition
	if c then first else other

manyWhile :: Parse token Bool -> Parse token x -> Parse token [x]
manyWhile check thing = go where
	go = test check (do
		x <- thing
		xs <- go
		return $ x : xs) (return [])


manyUntil :: Parse token Bool -> Parse token x -> Parse token [x]
manyUntil check thing = go where
	go = test check (return []) (do
		x <- thing
		xs <- go
		return $ x : xs)

crash :: Message -> Parse token result
crash msg = Parse $ const (0, Left msg)

check :: (token -> Bool) -> Parse (Locate token) Bool
check fun = do
	next <- peekMaybe
	case next of
		Nothing -> return $ False
		Just (Locate _at thing) -> return $ fun thing

{-
expectString :: String -> Message -> Message -> Parse at String Message ()
expectString string other end = Parse go where
	go [] = (0, Left end)
	go (Locate {value = t} : _)
		|t == string = (1, Right ())
		|otherwise = (0, Left other)

		-}


