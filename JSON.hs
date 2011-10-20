module JSON where

{-
    JSON.hs

    Implements RFC 4627, the JSON interchange format.

    TODO:
    1.  Implement the unicode characters as specified in the RFC.
    2.  Extension: enable single quote strings.
-}

import Control.Monad (liftM)
import Data.Char
import qualified Data.Map as DM
import Util

data JSONValue = JNum Int | JStr String | JBool Bool | JNull |
                 JArr [JSONValue] | JObj (DM.Map String JSONValue)

data Token = NUM String | LIT String | STR String | BARRAY | BOBJECT |
             EARRAY | EOBJECT | COMMA | COLON deriving Show

type Tokens = [Token]

tokenise :: Monad m => String -> m Tokens
tokenise ""       = return []
tokenise ('"':xs) = case drawString xs of
    Yes (str, rst) -> liftM ((STR str):) $ tokenise rst
    No x           -> fail x
tokenise (',':xs) = liftM (COMMA:)   $ tokenise xs
tokenise (':':xs) = liftM (COLON:)   $ tokenise xs
tokenise ('[':xs) = liftM (BARRAY:)  $ tokenise xs
tokenise (']':xs) = liftM (EARRAY:)  $ tokenise xs
tokenise ('{':xs) = liftM (BOBJECT:) $ tokenise xs
tokenise ('}':xs) = liftM (EOBJECT:) $ tokenise xs
tokenise s@(x:xs) | isSpace x = tokenise xs
                  | isDigit x = case drawNumber s of
    Yes (num, rst) -> liftM ((NUM num):) $ tokenise rst
    No y           -> fail y
                  | isAlpha x = case drawLiteral s of
    Yes (lit, rst) -> liftM ((LIT lit):) $ tokenise rst
    No y           -> fail y
                  | (x == '-') && (not $ null xs) && (isDigit $ head xs) =
    case drawNumber xs of
        Yes (num, rst) -> liftM ((NUM ("-" ++ num)):) $ tokenise rst
        No y           -> fail y
                  | otherwise = fail $ "Illegal character " ++ show x

drawString, drawLiteral, drawNumber :: Monad m => String -> m (String, String)
drawString = drs False "" where
    drs :: Monad m => Bool -> String -> String -> m (String, String)
    drs True  _ ""     = fail "Dangling escape"
    drs True  s (x:xs) = drs False (s ++ [escapee x]) xs
    drs False s ""        = fail "Unclosed quote"
    drs False s ('\\':xs) = drs True s xs
    drs False s ('"':xs)  = return (s, xs)
    drs False s (x:xs)    = drs False (s ++ [x]) xs

drawLiteral = drl "" where
    drl :: Monad m => String -> String -> m (String, String)
    drl s ""                   = return (s, "")
    drl s r@(x:xs) | isAlpha x = drl (s ++ [x]) xs
                   | otherwise = return (s, r)

drawNumber = drn "" where
    drn :: Monad m => String -> String -> m (String, String)
    drn s ""                   = return (s, "")
    drn s r@(x:xs) | isDigit x = drn (s ++ [x]) xs
                   | otherwise = return (s, r)

escapee :: Char -> Char
escapee 'n' = '\n'
escapee 't' = '\t'
escapee 'b' = '\b'
escapee 'f' = '\f'
escapee 'r' = '\r'
escapee x   = x

parse :: Monad m => [Token] -> m JSONValue
parse it@(BARRAY:xs) = case parseToks it of
    Yes (rez, _) -> return rez
    No         e -> fail e
parse it@(BOBJECT:xs) = case parseToks it of
    Yes (rez, _) -> return rez
    No         e -> fail e
parse _               = fail "JSON documents must be [arrays] or {objects}"

parseToks :: Monad m => Tokens -> m (JSONValue, Tokens)
parseToks ((NUM x):xs) = case getInt x of
    Nothing -> fail ("A malformed number, " ++ (show x) ++ ", was encountered")
    Just i  -> return (JNum i, xs)
parseToks ((STR s):xs) = return (JStr s, xs)
parseToks ((LIT l):xs) = case l of
    "true"  -> return (JBool True, xs)
    "false" -> return (JBool False, xs)
    "null"  -> return (JNull, xs)
    _       -> fail $ "Unrecognised literal: " ++ show l
parseToks (BARRAY:xs)  = case arrayElems xs of
    Yes (them, rst) -> return (JArr them, rst)
    No            e -> fail e
parseToks (BOBJECT:xs) = case objMembers xs of
    Yes (them, rst) -> return (JObj $ DM.fromList them, rst)
    No            e -> fail e
parseToks _            = fail "Missing element"

arrayElems :: Monad m => Tokens -> m ([JSONValue], Tokens)
arrayElems = ae_ [] where
    ae_ :: Monad m => [JSONValue] -> Tokens -> m ([JSONValue], Tokens)
    ae_  _          [] = fail "Array doesn't close"
    ae_ sf (EARRAY:xs) = return (sf, xs)
    ae_ sf rst         = case parseToks rst of
        Yes (stuff, tks) -> case postMemElem tks of
            Yes nxt -> ae_ (sf ++ [stuff]) nxt
            No  rsn -> fail rsn
        No             e -> fail e

objMembers :: Monad m => Tokens -> m ([(String, JSONValue)], Tokens)
objMembers = om_ [] where
    om_ :: Monad m => [(String, JSONValue)] -> Tokens -> m ([(String, JSONValue)], Tokens)
    om_  _           [] = fail "Object doesn't close"
    om_ sf (EOBJECT:xs) = return (sf, xs)
    om_ sf rst          = case parseToks rst of
        Yes (nom, tks) -> case pickMemRgt tks of
            Yes (val, mo) -> case postMemElem mo of
                Yes      nxt -> case rawString nom of
                    Yes     raw -> om_ ((raw, val):sf) nxt
                    No       e3 -> fail e3
                No        e2 -> fail e2
            No         e1 -> fail e1
        No           e -> fail e

pickMemRgt :: Monad m => Tokens -> m (JSONValue, Tokens)
pickMemRgt (COLON:xs) = parseToks xs
pickMemRgt _          = fail "Separate object pairs with a colon (:)"

rawString :: Monad m => JSONValue -> m String
rawString (JStr s) = return s
rawString _        = fail "Object member names should be strings"

postMemElem :: Monad m => Tokens -> m Tokens
postMemElem (COMMA:xs)      = return xs
postMemElem it@(EOBJECT:xs) = return it
postMemElem it@(EARRAY:xs)  = return it
postMemElem _               = fail "Put a comma between all elements"

instance Show JSONValue where
    show (JNum n)  = show n
    show (JStr s)  = show s
    show (JBool b) = if b then "true" else "false"
    show JNull     = "null"
    show (JArr ar)   = show ar
    show (JObj ob)   =
        let s = case DM.toList ob of {
            []     -> "";
            [x]    -> doPair x;
            (x:xs) -> (foldr (\y z -> z ++ (doPair y) ++ ", ") "" xs) ++
                doPair x} in "{" ++ s ++ "}" where
                doPair :: (String, JSONValue) -> String
                doPair (s, v) = (show s) ++ ":" ++ (show v)

toJSON :: Monad m => String -> m JSONValue
toJSON str = case tokenise str of
    Yes x -> case parse x of
        Yes y -> return y
        No  z -> fail z
    No  e -> fail e

(@@) :: Monad m => String -> JSONValue -> m JSONValue
p @@ j = fetch_ (pathify p) j where
    pathify :: String -> [String]
    pathify str =
        let (f, s) = break (== '/') str in
        (case f of
            "" -> []
            _  -> [f]) ++ if null s then [] else (pathify $ drop 1 s)

    fetch_ :: Monad m => [String] -> JSONValue -> m JSONValue
    fetch_ []     j = return j
    fetch_ (x:xs) j = lissez x j >>= fetch_ xs

lissez :: Monad m => String -> JSONValue -> m JSONValue
lissez mem json = (case getInt mem of
    Nothing -> (case json of
        JObj m -> (case DM.lookup mem m of
            Nothing -> fail $ (show mem) ++ " Not found"
            Just it -> return it)
        _      -> fail $ "Can't locate " ++ show mem ++ " in an array")
    Just n  -> (case json of
        JArr r -> (case getAt r n of
            Nothing -> fail $ show n ++ " is out of array bounds"
            Just it -> return it)
        _      -> fail $ "You're using ints as object keys? No."))

getInt :: Monad m => String -> m Int
getInt ('-':xs) = getInt xs >>= (\x -> return (-x))
getInt str =
    if and [isDigit x | x <- str] then return $ read str else fail "NaN"

getAt :: Monad m => [a] -> Int -> m a
getAt []     _ = fail "Out of bounds"
getAt (x:_)  0 = return x
getAt (x:xs) n = getAt xs (n - 1)
