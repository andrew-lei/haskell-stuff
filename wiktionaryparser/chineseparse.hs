module ChineseParse where

import Prelude hiding (unlines)
import Text.HTML.TagSoup
import Data.ByteString.Char8 (append, cons, ByteString, unlines)
import Data.ByteString.UTF8 (fromString)

headers :: [Tag String] -> [(String,[Char])]
headers (TagOpen "span" [("class","mw-headline"),("id",_)]:rest) = section rest ++ headers rest
headers (_:rest) = headers rest
headers [] = []

section :: [Tag String] -> [(String,[Char])]
section (TagOpen "tr" []:rest) = pinyin rest:section rest
section (TagClose "table":_)   = []
section (_:rest)               = section rest

pinyin :: [Tag String] -> (String, [Char])
pinyin (TagOpen "a" [("href", _),("title",py)]:_:rest) = (py,hanzi rest)
pinyin (_:rest)                                        = pinyin rest

hanzi:: [Tag String] -> [Char]
hanzi (TagText "\n":rest)    = hanzi rest
hanzi (TagText ", ":rest)    = hanzi rest
hanzi (TagText "\160:":rest) = hanzi rest
hanzi (TagText chichar:rest) = head chichar:hanzi rest
hanzi (TagClose "tr":_)      = []
hanzi (_:rest)               = hanzi rest

fromString' :: (String, [Char]) -> ByteString
fromString' (a, b) = fromString a `append` (',' `cons` fromString b)

parsePage :: String -> ByteString
parsePage = unlines . map fromString' . headers . parseTags
