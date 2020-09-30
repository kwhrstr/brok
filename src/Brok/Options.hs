{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Options where

import RIO 

import Brok.Parser.Options (options)
import Brok.Types.Next     (Next)

parse :: [Text] -> Either Text Next
parse = options
