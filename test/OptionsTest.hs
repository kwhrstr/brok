{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module OptionsTest where

import RIO 

import Test.Tasty
import Test.Tasty.HUnit

import Brok.Options      (parse)
import Brok.Types.Config
import Brok.Types.Next   (Next (..))

test_options :: TestTree
test_options =
    testGroup
        "Brok.Options"
        [ testGroup
              "Help"
              [ testCase "--help" (assertEqual "return Help" (Right Help) (parse ["--help"]))
              , testCase "-h" (assertEqual "return Help" (Right Help) (parse ["-h"]))
              ]
        , testGroup
              "Version"
              [ testCase
                    "--version"
                    (assertEqual "return Version" (Right Version) (parse ["--version"]))
              , testCase "-v" (assertEqual "return Version" (Right Version) (parse ["-v"]))
              ]
        , testCase
              "single file"
              (assertEqual
                   "gives back file"
                   (Right (Continue (defaultConfig {files = ["blah.md"]})))
                   (parse ["blah.md"]))
        , testCase
              "multiple files"
              (assertEqual
                   "gives back files"
                   (Right (Continue (defaultConfig {files = ["blah.md", "tests/spoon.md"]})))
                   (parse ["blah.md", "tests/spoon.md"]))
        , testCase
              "single file with only-failures option"
              (assertEqual
                   "gives back files"
                   (Right (Continue (defaultConfig {onlyFailures = True, files = ["blah.md"]})))
                   (parse ["--only-failures", "blah.md"]))
        , testCase
              "single file with no-colors option"
              (assertEqual
                   "gives back files"
                   (Right (Continue (defaultConfig {noColor = True, files = ["blah.md"]})))
                   (parse ["--no-color", "blah.md"]))
        , testCase
              "single file with check-certs option"
              (assertEqual
                   "gives back files"
                   (Right (Continue (defaultConfig {checkCerts = True, files = ["blah.md"]})))
                   (parse ["--check-certs", "blah.md"]))
        , testCase
              "single file with no-cache option"
              (assertEqual
                   "gives back files"
                   (Right (Continue (defaultConfig {cache = Nothing, files = ["blah.md"]})))
                   (parse ["--no-cache", "blah.md"]))
        , testCase
              "single file with cache option"
              (assertEqual
                   "gives back files"
                   (Right (Continue (defaultConfig {cache = Just 172800, files = ["blah.md"]})))
                   (parse ["--cache", "172800", "blah.md"]))
        , testCase
              "multiple files with cache option"
              (assertEqual
                   "gives back files"
                   (Right
                        (Continue
                             (defaultConfig
                              {cache = Just 172800, files = ["blah.md", "tests/spoon.md"]})))
                   (parse ["--cache", "172800", "blah.md", "tests/spoon.md"]))
        , testCase
              "multiple files with interval option"
              (assertEqual
                   "gives back files"
                   (Right
                        (Continue
                             (defaultConfig {interval = 200, files = ["blah.md", "tests/spoon.md"]})))
                   (parse ["--interval", "200", "blah.md", "tests/spoon.md"]))
        , testCase
              "multiple files with ignore option"
              (assertEqual
                   "gives back files"
                   (Right
                        (Continue
                             (defaultConfig
                              { ignore = ["http://www.google.com", "http://facebook.com"]
                              , files = ["blah.md", "tests/spoon.md"]
                              })))
                   (parse
                        [ "--ignore"
                        , "http://www.google.com"
                        , "http://facebook.com"
                        , "blah.md"
                        , "tests/spoon.md"
                        ]))
        , testCase
              "multiple files with all options #1"
              (assertEqual
                   "gives back files"
                   (Right
                        (Continue
                             (defaultConfig
                              { cache = Just 172800
                              , interval = 400
                              , ignore = ["http://www.google.com", "http://facebook.com"]
                              , files = ["blah.md", "tests/spoon.md"]
                              })))
                   (parse
                        [ "--cache"
                        , "172800"
                        , "--interval"
                        , "400"
                        , "--ignore"
                        , "http://www.google.com"
                        , "http://facebook.com"
                        , "blah.md"
                        , "tests/spoon.md"
                        ]))
        , testCase
              "multiple files with all options #2"
              (assertEqual
                   "gives back files"
                   (Right
                        (Continue
                             (defaultConfig
                              { cache = Just 172800
                              , interval = 400
                              , ignore = ["http://www.google.com", "http://facebook.com"]
                              , files = ["blah.md", "tests/spoon.md"]
                              })))
                   (parse
                        [ "--interval"
                        , "400"
                        , "--cache"
                        , "172800"
                        , "--ignore"
                        , "http://www.google.com"
                        , "http://facebook.com"
                        , "blah.md"
                        , "tests/spoon.md"
                        ]))
        , testCase
              "multiple files with all options #3"
              (assertEqual
                   "gives back files"
                   (Right
                        (Continue
                             (defaultConfig
                              { cache = Just 172800
                              , interval = 400
                              , ignore = ["http://www.google.com", "http://facebook.com"]
                              , files = ["blah.md", "tests/spoon.md"]
                              })))
                   (parse
                        [ "--ignore"
                        , "http://www.google.com"
                        , "http://facebook.com"
                        , "--interval"
                        , "400"
                        , "--cache"
                        , "172800"
                        , "blah.md"
                        , "tests/spoon.md"
                        ]))
        , testCase
              "files with space"
              (assertEqual
                   "gives back files"
                   (Right (Continue (defaultConfig {files = ["blah.md", "tests/spoon book.md"]})))
                   (parse ["blah.md", "tests/spoon book.md"]))
        ]
