{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Parser.DBTest where

import RIO 
import RIO.List.Partial (last)
import Test.Tasty
import Test.Tasty.HUnit

import Data.Either    (fromRight)
import Data.FileEmbed (embedFile)

import Brok.Parser.DB (db)

content :: Text
content = decodeUtf8Lenient $(embedFile "test/data/.brokdb")

invalid :: Text
invalid = decodeUtf8Lenient $(embedFile "test/data/.brokdb-invalid")

big :: Text
big = decodeUtf8Lenient $(embedFile "test/data/.brokdb-big")

test_db :: TestTree
test_db =
    testGroup
        "Brok.Parser.DB"
        [ testCase
              "parses .brokdb"
              (assertEqual
                   "Gives back URLs with timestamp"
                   (Right
                        [ ( "https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle"
                          , 1547542131)
                        , ( "https://www.amazon.co.uk/Code-Language-Computer-Hardware-Software/dp/0735611319"
                          , 1546959165)
                        , ("https://rosettacode.org/wiki/FizzBuzz", 1546440765)
                        , ( "https://medium.com/dailyjs/the-why-behind-the-wat-an-explanation-of-javascripts-weird-type-system-83b92879a8db"
                          , 1546008765)
                        ])
                   (db content))
        , testCase
              "big file (last)"
              (assertEqual
                   "Gives back final url"
                   "https://developmentarc.gitbooks.io/react-indepth/content/life_cycle/the_life_cycle_recap.html"
                   (fst (last . fromRight [] $ db big)))
        , testCase
              "big file (length)"
              (assertEqual "Gives back length" 142 (length $ fromRight [] (db big)))
        , testCase "invalid .brokdb" (assertEqual "Parse error" True (isLeft $ db invalid))
        , testCase "parses empty" (assertEqual "Gives back empty array" (Right []) (db ""))
        ]
