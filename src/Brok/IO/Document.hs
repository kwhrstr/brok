{-# LANGUAGE NoImplicitPrelude #-}

module Brok.IO.Document
    ( TFilePath
    , readContent
    ) where

import RIO
import RIO.Directory


import Brok.Types.Brok     (Brok)
import Brok.Types.Document

readContent :: TFilePath -> Brok Document
readContent path = do
    exists <- doesFileExist path
    if exists
        then withContent path <$> readFileUtf8 path
        else pure $ notFound path
