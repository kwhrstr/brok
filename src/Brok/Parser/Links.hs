{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.Parser.Links
    ( links
    , url
    ) where

import RIO
import Data.Attoparsec.Text
import Data.List            (nub)

import Brok.Parser.Attoparsec
import Brok.Types.URL         (URL)

type Token = Maybe URL

preQueryChars :: String
preQueryChars = "-._~:/#%@"

queryBodyChars :: String
queryBodyChars = preQueryChars <> "!$&*+,;="

chars :: String -> Parser Char
chars chrs = digit <|> letter <|> choice (char <$> chrs)

-- parentheses
parens :: Parser Text -> Parser Text
parens parser = surround '(' ')' parser <|> surround '[' ']' parser

-- urls
part :: String -> Parser Text
part str = mconcat <$> many1 (parens (part str) <|> manyChars (chars str))

query :: Parser Text
query = (<>) <$> string "?" <*> part queryBodyChars

url :: Parser Text
url =
    concat5 <$> string "http" <*> chopt 's' <*> string "://" <*> part preQueryChars <*>
    option "" query

noise :: Parser Token
noise = anyChar $> Nothing

urls :: Parser [URL]
urls = nub . catMaybes <$> many1 ((Just <$> url) <|> noise)

-- run parser
links :: Text -> Either Text [URL]
links ""      = Right []
links content = first tshow $ parseOnly urls content
