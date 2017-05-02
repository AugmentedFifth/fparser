module Fparser
    ( parseOut
    ) where

import           Control.Monad

import           Data.Array
import           Data.Char
import           Data.List
import           Data.Maybe

import           System.Environment
import           System.Exit
import           System.IO

import           Text.Regex.TDFA


decimallvlre :: Regex
decimallvlre = makeRegex "^[1-9][0-9]*\\.( |$)"
letterlvlre :: Regex
letterlvlre = makeRegex "[a-z]\\.( |$)"
numerallvlre :: Regex
numerallvlre = makeRegex "\\([ivxlc]+\\)( |$)"
lvlres :: [Regex]
lvlres = [decimallvlre, letterlvlre, numerallvlre]

htmlBoiler :: [String]
htmlBoiler =
    [ "<!DOCTYPE html>"
    , "<html>"
    , "    <head>"
    , "        <meta charset=\"utf-8\">"
    , "        <title></title>"
    , "        <link href=\"https://fonts.googleapis.com/css?family=PT+Serif|Open+Sans\" rel=\"stylesheet\">"
    , "        <link rel=\"stylesheet\" href=\"fparser.css\">"
    , "        <script src=\"https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS_CHTML\" async></script>"
    , "        <script src=\"smartquotes.min.js\" async></script>"
    , "    </head>"
    , "    <body onload=\"smartquotes();\">\n"
    ]

numeralvalues :: Integral i => [(String, i)]
numeralvalues =
    [ ("iv", 4)
    , ("ix", 9)
    , ("xl", 40)
    , ("xc", 90)
    , ("i",  1)
    , ("v",  5)
    , ("x",  10)
    , ("l",  50)
    , ("c",  100)
    ]


data Tree a = Root [Tree a]
            | Node a [Tree a]
            deriving (Show)


parseOut :: IO ()
parseOut = do
    args <- getArgs
    let filename = parseArgs args
    when (null filename) exitFailure
    contents <- readFile filename
    let htmlfilename = htmlFilename filename
    handle <- openFile htmlfilename WriteMode
    hPutStr handle (convert contents)
    hClose handle
    putStrLn ("Successfully wrote " ++ htmlfilename)


parseArgs :: [String] -> String
parseArgs (fileName:_) = fileName
parseArgs _            = ""


htmlFilename :: String -> String
htmlFilename fname =
    if '.' `elem` fname then
        reverse (dropWhile (/= '.') (reverse fname)) ++ "html"
    else
        fname ++ ".html"


convert :: String -> String
convert contents =
    toHtml (parse indent contents)
    where indent = getIndent contents


getIndent :: String -> String
getIndent contents =
    let clines = lines contents
        leadingwsline =
            find (\l ->
                     case l of
                         (c:_) -> isWs c
                         _     -> False)
                 clines
    in  takeWhile isWs (fromMaybe " " leadingwsline)


isWs :: Char -> Bool
isWs = (`elem` " \t")


parse :: String -> String -> Tree (String, String)
parse indent contents =
    let root = Root []
        elines = foldLines indent (lines contents)
        insLine tree (i, lvlcontent, linecontent) =
            treeIns i (lvlcontent, linecontent) tree
    in  foldl' insLine root elines


foldLines :: String -> [String] -> [(Int, String, String)]
foldLines indent = foldl' (\accu l ->
    let extracted = extractContent indent l
    in  case extracted of
        (indentlvl, "", "") ->
            if null accu then
                accu ++ [(indentlvl, "", "")]
            else
                let prev = last accu
                in  case prev of
                    (previlvl, "", _) -> accu ++ [(previlvl,     "", "")]
                    (previlvl, _,  _) -> accu ++ [(previlvl + 1, "", "")]
        e -> accu ++ [e])
    []


extractContent :: String -> String -> (Int, String, String)
extractContent indent line =
    let indentlen = length indent
        chopped = span isWs line
        indentlvl = (`quot` indentlen) . length . fst $ chopped
        content = snd chopped
        lvlextract = extractLvl content
        format = inlineTex . formatHeader . strip " \t"
    in  (indentlvl, fst lvlextract, format (snd lvlextract))


inlineTex :: String -> String
inlineTex s =
    fst (foldr (\c (str, b) ->
                   if c == '`' then
                       ('\\' : (b ? '(' $ ')') : str, not b)
                   else
                       (c : str, b))
               ("", False)
               s)


formatHeader :: String -> String
formatHeader s =
    if headernum > 0 then
           "<h"
        ++ show headernum
        ++ ">"
        ++ lstrip " \t" headercontent
        ++ "</h"
        ++ show headernum
        ++ ">"
    else
        s
    where
        headercontent = dropWhile (== '#') s
        headernum = length s - length headercontent


strip :: String -> String -> String
strip tostrip = rstrip tostrip . lstrip tostrip


lstrip :: String -> String -> String
lstrip tostrip = dropWhile (`elem` tostrip)


rstrip :: String -> String -> String
rstrip tostrip =
    foldr (\c accu ->
              null accu && c `elem` tostrip ? accu $ c : accu)
          ""


extractLvl :: String -> (String, String)
extractLvl content =
    case mregex of
        Nothing    -> ("", content)
        Just regex ->
            case mmatch of
                Nothing  -> ("", content)
                Just mch ->
                    let fullmatch = mch ! 0
                    in  splitAt (snd fullmatch - fst fullmatch) content
            where mmatch = matchOnce regex content
    where mregex = find (`matchTest` content) lvlres


treeIns :: Integral i => i -> a -> Tree a -> Tree a
treeIns 0 x tree                  = addChild tree x
treeIns _ x (Node content [])     = addChild (Node content []) x
treeIns i x (Node content (c:cs)) = Node content (treeIns (i - 1) x c : cs)
treeIns _ x (Root [])             = addChild (Root []) x
treeIns i x (Root (c:cs))         = Root (treeIns (i - 1) x c : cs)


addChild :: Tree a -> a -> Tree a
addChild (Node content children) x = Node content (Node x [] : children)
addChild (Root children) x         = Root (Node x [] : children)


toHtml :: Tree (String, String) -> String
toHtml tree =
    let boiler = intercalate "\n" htmlBoiler
        indent = replicate 4 ' '
    in  boiler ++ treeToHtml indent (2 :: Int) tree ++ "    </body>\n</html>\n"


treeToHtml :: Integral i => String -> i -> Tree (String, String) -> String
treeToHtml indent indentlvl (Node ("", content) children) =
    let baseindent = replicatenate indentlvl indent
        thiscontent = baseindent
                   ++ (null content ? "<br />" $ content)
                   ++ "\n"
        recurse = treeToHtml indent (indentlvl + 1)
    in  thiscontent
     ++ (null children ? "" $ baseindent ++ "\\[\n")
     ++ concatMap recurse (reverse children)
     ++ (null children ? "" $ baseindent ++ "\\]\n")
treeToHtml indent indentlvl (Node (lvlstr, content) children) =
    let lvlstrandstart = lvlStrType lvlstr
        baseindent = replicatenate indentlvl indent
        recurse = treeToHtml indent (indentlvl + 1)
    in  baseindent
     ++ "<ol start=\""
     ++ show (snd lvlstrandstart)
     ++ "\" type=\""
     ++ fst lvlstrandstart
     ++ "\">\n"
     ++ baseindent
     ++ indent
     ++ "<li>\n"
     ++ (null content ? "" $ baseindent ++ indent ++ content ++ "\n")
     ++ concatMap recurse (reverse children)
     ++ baseindent
     ++ indent
     ++ "</li>\n"
     ++ baseindent
     ++ "</ol>\n"
treeToHtml indent indentlvl (Root children) =
    concatMap (treeToHtml indent indentlvl) (reverse children)


infixr 1 ?
(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y


lvlStrType :: String -> (String, Int)
lvlStrType lvlstr =
    let mlvltypei = findIndex (`matchTest` lvlstr) lvlres
    in  case mlvltypei of
        Just 0 -> ("1", decimalOrd lvlstr)
        Just 1 -> ("a", alphaOrd lvlstr)
        Just 2 -> ("i", numeralOrd (strip "() \t" lvlstr))
        _      -> ("1", 1)


decimalOrd :: String -> Int
decimalOrd = read . takeWhile (`elem` ['0'..'9'])


alphaOrd :: String -> Int
alphaOrd (c:_) = ord c - 96
alphaOrd _     = -1


numeralOrd :: Integral i => String -> i
numeralOrd s =
    let fstIsPrefix numeralval = fst numeralval `isPrefixOf` s
        mprefixmatch = find fstIsPrefix numeralvalues
    in  case mprefixmatch of
        Just (numstr, val) -> val + numeralOrd (genericDrop (length numstr) s)
        _                  -> 0


replicatenate :: Integral i => i -> [a] -> [a]
replicatenate n l = concat $ genericReplicate n l
