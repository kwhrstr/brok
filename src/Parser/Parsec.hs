{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Parsec
    ( Parser
    , alphaNum
    , anyToken
    , char
    , many1
    , oneOf
    , optionMaybe
    , parse
    , string
    , try1
    , text
    , tchar
    , chopt
    , surround
    , concat3
    , concat4
    ) where

import ClassyPrelude hiding (try)

import Text.Parsec      (alphaNum, anyToken, char, many1, oneOf, optionMaybe, parse, string, try)
import Text.Parsec.Text (Parser)

try1 :: Parser a -> Parser a
try1 = try

text :: String -> Parser Text
text str = pack <$> string str

tchar :: Char -> Parser Text
tchar ch = singleton <$> char ch

chopt :: Char -> Parser Text
chopt ch = maybe "" singleton <$> optionMaybe (char ch)

surround :: Char -> Char -> Parser Text -> Parser Text
surround open close parser = concat3 <$> tchar open <*> parser <*> tchar close

concat3 :: (Monoid a) => a -> a -> a -> a
concat3 t1 t2 t3 = concat [t1, t2, t3]

concat4 :: (Monoid a) => a -> a -> a -> a -> a
concat4 t1 t2 t3 t4 = concat [t1, t2, t3, t4]