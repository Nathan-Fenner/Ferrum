
module Parse.Core where

import Location
import Control.Applicative
import Lex


newtype Parse result = Parse { run :: [Locate Token] -> (Int, Either Message result ) }

instance Functor (Parse) where
	fmap f (Parse g) = Parse $ \i -> let (a, r) = g i in case r of
		Left msg -> (a, Left msg)
		Right result -> (a, Right $ f result)

instance Applicative (Parse) where
	pure x = Parse $ \i -> (0, Right x)
	
	Parse f <*> Parse g = Parse $ \i -> let (af, rf) = f i in case rf of
		Left msg -> (af, Left msg)
		Right resultFun -> let (ag, rg) = g (drop af i) in case rg of
			Left msg' -> (af + ag, Left msg')
			Right resultArg -> (af + ag, Right $ resultFun resultArg)
		where


instance Monad (Parse) where
	return x = pure x
	Parse f >>= into = Parse $ \i -> let (af, rf) = f i in case rf of
		Left msg -> (af, Left msg)
		Right thing -> let Parse q = into thing; (aq, rq) = q (drop af i) in (aq + af, rq)

newtype Message = Message String deriving Show

advance :: Int -> Parse ()
advance n = Parse $ const (n, Right ())

peekLocation :: FilePath -> [Locate token] -> Location
peekLocation file [] = End file
peekLocation _file (Locate {at = loc} : _) = loc

peekMaybe :: Parse (Maybe (Locate Token))
peekMaybe = Parse $ \i -> case i of
	[] -> (0, Right Nothing)
	(token : _) -> (0, Right $ Just token)

peek :: Message -> Parse (Locate Token)
peek msg = Parse $ \i -> case i of
	[] -> (0, Left msg)
	(token : _) -> (0, Right $ token)

askMaybe :: Parse (Maybe (Locate Token))
askMaybe = do
	token <- peekMaybe
	advance 1
	return token

ask :: Message -> Parse (Locate Token)
ask msg = do
	token <- peek msg
	advance 1
	return token

test :: Parse Bool -> Parse x -> Parse x -> Parse x
test condition first other = do
	c <- condition
	if c then first else other

manyWhile :: Parse Bool -> Parse x -> Parse [x]
manyWhile check thing = go where
	go = test check (do
		x <- thing
		xs <- go
		return $ x : xs) (return [])


manyUntil :: Parse Bool -> Parse x -> Parse [x]
manyUntil check thing = go where
	go = test check (return []) (do
		x <- thing
		xs <- go
		return $ x : xs)

manyMaybe :: Parse (Maybe x) -> Parse [x]
manyMaybe fun = do
	x <- fun
	case x of
		Nothing -> return []
		Just t -> do
			r <- manyMaybe fun
			return $ t : r

crash :: Message -> Parse result
crash msg = Parse $ const (0, Left msg)

check :: (Token -> Bool) -> Parse Bool
check fun = do
	next <- peekMaybe
	case next of
		Nothing -> return $ False
		Just (Locate _at thing) -> return $ fun thing

expectAt :: Token -> Message -> Parse Location
expectAt token message = do
	at <- expectMaybe token
	case at of
		Just x -> return x
		Nothing -> crash message

expect :: Token -> Message -> Parse ()
expect token message = expectAt token message >> return ()

checkNext :: Token -> Parse Bool
checkNext token = do
	x <- peekMaybe
	case x of
		Just (Locate _ t) -> return $ t == token
		_ -> return False

checkEnd :: Parse Bool
checkEnd = do
	next <- peekMaybe
	case next of
		Nothing -> return True
		_ -> return False

expectMaybe :: Token -> Parse (Maybe Location)
expectMaybe token = do
	next <- peekMaybe
	case next of
		Just (Locate at t) -> if t == token then advance 1 >> return (Just at) else return Nothing
		_ -> return Nothing

expectName :: Message -> Parse Name
expectName message = do
	next <- peekMaybe
	case next of
		Just (Locate at (TWord name)) -> do
			advance 1
			return $ Locate at name
		_ -> crash message