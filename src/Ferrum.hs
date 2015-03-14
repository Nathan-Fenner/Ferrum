
import Parse.Core


getLetter :: Parse (Locate at String) Message (Locate at String)
getLetter = do
	Locate at p <- peek $ Message "expected `A` or `B` or `C`"
	if p == "A" || p == "B" || p == "C" then do
		advance 1
		return $ Locate at p
		else crash $ Message "expected `A` or `B` or `C`"

getLetters :: Parse (Locate at String) Message [Locate at String]
getLetters = do
	manyUntil (check (== "end")) getLetter

wrap :: [String] -> [Locate Int String]
wrap list = zipWith Locate (map Location [0..]) list

main = do
	print $ run getLetters $ wrap $ words "A B C B A Q end"