{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Brok
    ( brok
    ) where

--import ClassyPrelude
import RIO 
import Data.FileEmbed (embedFile)
import Data.Text.IO   (hPutStrLn, putStrLn)
import System.Environment (getArgs)
import qualified RIO.Text as T


import           Data.Version               (showVersion)
import           Language.Haskell.TH.Syntax (liftString)
import qualified Paths_brok                 (version)

import           Brok.IO.CLI         (header, replace)
import           Brok.IO.DB          (getCached, setCached)
import           Brok.IO.Document    (readContent)
import           Brok.IO.Http        (mkManager)
import           Brok.IO.Output      (output)
import           Brok.Options        (parse)
import           Brok.Types.Brok     (Brok, appConfig, mkApp)
import qualified Brok.Types.Config   as C (checkCerts, files, ignore, onlyFailures)
import           Brok.Types.Document (cachedLinks, checkLinks, ignoredLinks, justLinks, parseLinks)
import           Brok.Types.Link     (getURL, isSuccess)
import           Brok.Types.Next     (Next (..))
import Data.Text.Encoding (decodeUtf8)

go :: Brok ()
go = do
    config <- asks appConfig
    -- read files
    content <- traverse (readContent . T.unpack) (C.files config)
    -- find links in each file
    let parsed = parseLinks <$> content
    -- check cached successes
    cached <- getCached
    let uncached = cachedLinks cached . ignoredLinks (C.ignore config) <$> parsed
    -- check links in each file
    header "Checking URLs"
    liftIO $ putStrLn ""
    checked <- checkLinks uncached
    replace "Fetching complete"
    -- display results
    liftIO $ putStrLn ""
    header "Documents"
    anyErrors <- output (C.onlyFailures config) checked
    -- cache successes
    setCached $ getURL <$> filter isSuccess (concat (justLinks <$> checked))
    -- exit with appropriate status code
    liftIO $
        if anyErrors
            then void exitFailure
            else void exitSuccess

putHelp :: IO ()
putHelp = putStrLn $ decodeUtf8 $(embedFile "template/usage.txt")

putVersion :: IO ()
putVersion = putStrLn $ "brök " <> $(liftString $ showVersion Paths_brok.version)

-- entry point
brok :: IO ()
brok = do
    config <- parse <$> fmap (map T.pack) getArgs
    case config of
        Right (Continue cnf) -> do
            manager <- mkManager (C.checkCerts cnf)
            runRIO (mkApp cnf manager) go 
        Right Help -> putHelp
        Right Version -> putVersion
        Left _ -> do
            hPutStrLn stderr "Invalid format"
            putHelp
            void exitFailure
