
module Parse.Core where

import Location
import Control.Applicative



newtype Parse token msg result = Parse { run :: [token] -> (Int, Either msg result ) }

instance Functor (Parse token msg) where
	fmap f (Parse g) = Parse $ \i -> let (a, r) = g i in case r of
		Left msg -> (a, Left msg)
		Right result -> (a, Right $ f result)

instance Applicative (Parse token msg) where
	pure x = Parse $ \i -> (0, Right x)
	
	Parse f <*> Parse g = Parse $ \i -> let (af, rf) = f i in case rf of
		Left msg -> (af, Left msg)
		Right resultFun -> let (ag, rg) = g (drop af i) in case rg of
			Left msg' -> (af + ag, Left msg')
			Right resultArg -> (af + ag, Right $ resultFun resultArg)
		where


instance Monad (Parse token msg) where
	return x = pure x
	Parse f >>= into = Parse $ \i -> let (af, rf) = f i in case rf of
		Left msg -> (af, Left msg)
		Right thing -> let Parse q = into thing; (aq, rq) = q (drop af i) in (aq + af, rq)

newtype Message = Message String deriving Show

advance :: Int -> Parse x msg ()
advance n = Parse $ const (n, Right ())

peekLocation :: [Locate token] -> Location
peekLocation [] = End
peekLocation (Locate {at = loc} : _) = loc


expectString :: String -> Message -> Parse (Locate String) Message ()
expectString string message = Parse $ \i -> case i of
	[] -> (0, Left message)
	(Locate m t : _) -> if t == string then
		(1, Right ())
		else (0, Left message)

peekMaybe :: Parse token msg (Maybe token)
peekMaybe = Parse $ \i -> case i of
	[] -> (0, Right Nothing)
	(token : _) -> (0, Right $ Just token)

peek :: msg -> Parse (Locate token) msg (Locate token)
peek msg = Parse $ \i -> case i of
	[] -> (0, Left msg)
	(token : _) -> (0, Right $ token)

askMaybe :: Parse token msg (Maybe token)
askMaybe = do
	token <- peekMaybe
	advance 1
	return token

ask :: msg -> Parse (Locate token) msg (Locate token)
ask msg = do
	token <- peek msg
	advance 1
	return token

test :: Parse token msg Bool -> Parse token msg x -> Parse token msg x -> Parse token msg x
test condition first other = do
	c <- condition
	if c then first else other

manyWhile :: Parse token msg Bool -> Parse token msg x -> Parse token msg [x]
manyWhile check thing = go where
	go = test check (do
		x <- thing
		xs <- go
		return $ x : xs) (return [])


manyUntil :: Parse token msg Bool -> Parse token msg x -> Parse token msg [x]
manyUntil check thing = go where
	go = test check (return []) (do
		x <- thing
		xs <- go
		return $ x : xs)

crash :: msg -> Parse token msg result
crash msg = Parse $ const (0, Left msg)

check :: (token -> Bool) -> Parse (Locate token) msg Bool
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


