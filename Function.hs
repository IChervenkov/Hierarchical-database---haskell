module Function where

import Data.List ( elemIndex, isInfixOf )
import System.IO ()
import Control.DeepSeq ( NFData(rnf) )
import Data.Char ( digitToInt )
import Data.Maybe ( isNothing )

data Element =
    NameElem String
    | ValueElem [String]
    | Attr (Maybe (Attribut, Attribut))
    deriving (Eq, Show)

data Attribut =
    NameAttr String
    | ValueAttr String
    deriving (Eq, Show)

removeASpaceList :: [String] -> [String]
removeASpaceList (x:xs) =
    let
        removeASpace :: String -> String
        removeASpace (' ':ns) = removeASpace ns
        removeASpace ns = ns
    in removeASpace x : removeASpaceList xs
removeASpaceList _ = []


readXMLFile :: String -> IO [String]
readXMLFile path = do
    input <- readFile path
    let formateInput = removeASpaceList (lines input)
    return formateInput

checkElemValue :: String -> Bool
checkElemValue (x:xs)
    | x == '>' && not (null xs) = True
    | otherwise = checkElemValue xs
checkElemValue _ = False

getElemValue :: String -> [String] -> [String]
getElemValue str (x:xs)
    | checkElemValue str = [take (length (drop (length (getElemName str) + 2) str) - (length (getElemName str) + 3)) (drop (length (getElemName str) + 2) str)]
    | getElemName x == getElemName str = getElemValue str xs
    | getElemName x /= ("/" ++ getElemName str) = x:getElemValue str xs
    | otherwise = []
getElemValue _ _ = []


getElemName :: String -> String
getElemName x
    | not (checkElemValue x) = ptr ' ' (filter (\n -> n/='>' && n /= '<') x)
    | otherwise = ptr '>' (filter (/= '<') x)
    where
        ptr :: Char -> String -> String
        ptr ch (x:xs)
            | x /= ch = x:ptr ch xs
            | otherwise = ptr ch []
        ptr  _ _ = []

getAttribute :: String -> Maybe (Attribut,Attribut)
getAttribute x
    | "/" `isInfixOf` x = Nothing
    | length (words x) > 1 = Just (NameAttr (prin '=' (words x!!1)), ValueAttr (take (length (drop (length (prin '=' (words x !! 1)) + 3) (words x !! 1)) - 1) (drop (length (prin '=' (words x!!1)) + 2) (words x!!1))))
    | otherwise = Nothing
    where
        prin :: Char -> String -> String
        prin str (n:ns)
            | n == str = []
            | otherwise = n:prin str ns
        prin _ _ = []


makeElem :: [(String, String)] -> [String]
makeElem ((x,y):xs) = ("<" ++ x ++ ">" ++ y ++ "</" ++ x ++ ">") : makeElem xs
makeElem _ = []

wordsWithChar :: (Char -> Bool) -> String -> [String]
wordsWithChar p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWithChar p s''
                            where (w, s'') = break p s'

makeAttr :: [(String, String)] -> Maybe (Attribut,Attribut)
makeAttr [] = Nothing
makeAttr [("none", "none")] = Nothing
makeAttr ((x,y):xs) = Just (NameAttr x,ValueAttr y)

add :: String -> [String] -> String -> String -> [String] -> [String]
add n [] "" "" [x] = ("</" ++ n ++ ">") : [x]
add n (v:vs) "" "" [x] = v : add n vs "" "" [x]
add n (v:vs) nameAtr valAtr [x]
    | nameAtr /= "" && valAtr /= "" = ("<" ++ n ++ " "++ nameAtr ++"='"++ valAtr ++"'>") : add n (v:vs) "" "" [x]
    | otherwise = ("<" ++ n ++ ">") : add "" (v : vs) "" "" [x]
add n v "" "" (x:xs)
    | length xs == 1 = x : ("<" ++ n ++ ">") : add n v "" "" xs
    | otherwise = x : add n v "" "" xs
add n v nameAtr valAtr (x:xs) =  x : add n v nameAtr valAtr xs
add _ _ _ _ [] = []

createElem :: (Element,Element,Element) -> String -> IO String
createElem (NameElem name,ValueElem val, Attr Nothing) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (add name val "" "" input))
    return "This item is set up!"

createElem (NameElem name,ValueElem val, Attr (Just (NameAttr nameAttr,ValueAttr valAttr))) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (add name val nameAttr valAttr input))
    return "This item is set up!"

createElem _ _ = do 
    return "Error, this item cannot be created!"



readIndex :: String -> Maybe Int
readIndex (x:y:xs)
    | x == '[' = Just (digitToInt y)
    | otherwise = readIndex (y:xs)
readIndex _ = Nothing

readPath :: String -> String
readPath (x:xs)
    | x /= '/' = readPath xs
    | otherwise = xs
readPath _ = []

readAttr :: String -> String
readAttr (x:xs)
    | x == '@' = take (length xs - 1) xs
    | otherwise = readAttr xs
readAttr _ = ""

checkEqNameAttr :: String ->  Maybe (Attribut,Attribut) -> Bool
checkEqNameAttr attr (Just (NameAttr name,ValueAttr val))
    | attr == name = True
    | otherwise = False
checkEqNameAttr _ _ = False

getNameVal :: String -> Bool -> String
getNameVal (x:xs) check
    | x == '(' && not check = getNameVal xs True
    | check && x /= ')' = x : getNameVal xs True
    | otherwise = getNameVal xs False
getNameVal _ _ = []

readElem :: String -> Int -> [String] -> [String]
readElem _ _ [] = []
readElem str n (y:ys)
    | checkEqNameAttr (readAttr str) (getAttribute y) = getAttrVal (getAttribute y) : readElem str n ys
    | isNothing (readIndex str) && getElemName y == readPath str && getNameVal str False == "" = getElemValue y (y:ys) ++ readElem str n ys
    | readIndex str == Just n && getElemName y == readPath str = getElemValue y (y:ys)
    | readIndex str /= Just n && getElemName y == readPath str  = readElem str (succ n) ys
    | otherwise = readElem str n ys
    where
        getAttrVal :: Maybe (Attribut,Attribut) -> String
        getAttrVal (Just (NameAttr name, ValueAttr val)) = val
        getAttrVal _ = ""

searche :: String -> String -> IO [String]
searche path pathFile = do
    input <- readXMLFile pathFile
    return (if readElem path 0 input `isInfixOf` [] then ["Error, this element does not exist!"] else readElem path 0 input)



removeList :: [String] -> [String] -> [String]
removeList (x:xs) (y:ys) | x == y    = removeList xs ys
    | otherwise = y : removeList (x:xs) ys
removeList [] ys = ys
removeList _ _ = []

get :: String -> [String] -> String -> String -> [String] -> [String]
get _ _ _ _ [] = []
get nameElm valElm "" "" (x:xs)
    | getElemName x == nameElm && getElemValue x (x:xs) `isInfixOf` valElm && length valElm > 1 = x : valElm ++ ["</" ++ getElemName x ++ ">"] ++ get nameElm valElm "" "" xs
    | getElemName x == nameElm && getElemValue x (x:xs) `isInfixOf` valElm && length valElm == 1 = [x]
    | otherwise = get nameElm valElm "" "" xs

get nameElm valElm nameAttr valAttr (x:xs)
    | getElemName x == nameElm && getElemValue x (x:xs) `isInfixOf` valElm && length valElm > 1 && getAttribute x == Just (NameAttr nameAttr, ValueAttr valAttr) = x : valElm ++ ["</" ++ getElemName x ++ ">"] ++ get nameElm valElm nameAttr valAttr xs
    | getElemName x == nameElm && getElemValue x (x:xs) `isInfixOf` valElm && length valElm == 1 && getAttribute x == Just (NameAttr nameAttr, ValueAttr valAttr) = [x]
    | otherwise = get nameElm valElm nameAttr valAttr xs

deleteElem :: (Element, Element, Element) -> String -> IO String
deleteElem (NameElem name, ValueElem val, Attr Nothing) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (removeList (get name val "" "" input) input))
    return "This item has been deleted!"

deleteElem (NameElem name, ValueElem val, Attr (Just (NameAttr nameAttr, ValueAttr valAttr))) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (removeList (get name val nameAttr valAttr input) input))
    return "This item has been deleted!"

deleteElem _ _ = do
    return "Error, this item does not exist!"



updateElem :: (Element, Element, Element) -> (Element, Element, Element) -> String -> IO String
updateElem (NameElem name1, ValueElem val1, Attr Nothing) (NameElem name2, ValueElem val2, Attr Nothing) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (add name2 val2 "" "" (removeList (get name1 val1 "" "" input) input)))
    return "This item has been updated!"

updateElem (NameElem name1, ValueElem val1, Attr (Just (NameAttr nameAttr, ValueAttr valAttr))) (NameElem name2, ValueElem val2, Attr Nothing) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (add name2 val2 "" "" (removeList (get name1 val1 nameAttr valAttr input) input)))
    return "This item has been updated!"

updateElem (NameElem name1, ValueElem val1, Attr Nothing) (NameElem name2, ValueElem val2, Attr (Just (NameAttr nameAttr, ValueAttr valAttr))) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (add name2 val2 nameAttr valAttr (removeList (get name1 val1 "" "" input) input)))
    return "This item has been updated!"

updateElem (NameElem name1, ValueElem val1, Attr (Just (NameAttr nameAttr1, ValueAttr valAttr1))) (NameElem name2, ValueElem val2, Attr (Just (NameAttr nameAttr2, ValueAttr valAttr2))) path = do
    input <- readXMLFile path
    rnf input `seq` writeFile path (unlines (add name2 val2 nameAttr2 valAttr2 (removeList (get name1 val1 nameAttr1 valAttr1 input) input)))
    return "This item has been updated!"

updateElem _ _ _ = do
    return "Error, the item is not updated!"
