{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.DB
    ( getCached
    , setCached
    ) where

--import ClassyPrelude
import RIO
import RIO.Directory
import Data.Either           (fromRight)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified RIO.Text as T 
import Brok.Parser.DB    (db)
import Brok.Types.Brok   (Brok, appConfig)
import Brok.Types.Config (cache)
import Brok.Types.URL    (URL)

path :: String
path = ".brokdb"

-- time stuff
removeOld :: Integer -> [(URL, Integer)] -> Brok [(URL, Integer)]
removeOld age cached = do
    timestamp <- liftIO getPOSIXTime
    pure $ filter ((\val -> timestamp - val < fromInteger age) . fromInteger . snd) cached

stamp :: URL -> Brok (URL, Integer)
stamp lnk = do
    timestamp <- liftIO $ round <$> getPOSIXTime
    pure (lnk, timestamp)

-- write db
linkToText :: (URL, Integer) -> Text
linkToText (lnk, int) = T.concat [lnk, " ", textDisplay int]

write :: [(URL, Integer)] -> Brok ()
write links = writeFileUtf8 path . T.unlines $ linkToText <$> links

setCached :: [URL] -> Brok ()
setCached links = do
    mAge <- cache <$> asks appConfig
    case mAge of
        Nothing -> pure ()
        Just age -> do
            current <- load age
            stamped <- traverse stamp links
            write $ current <> stamped

-- read db
read :: Integer -> FilePath -> Brok [(URL, Integer)]
read age filepath = removeOld age . fromRight [] . db =<< readFileUtf8 filepath

load :: Integer -> Brok [(URL, Integer)]
load age = do
    exists <- doesFileExist path
    if exists
        then read age path
        else pure []

getCached :: Brok [URL]
getCached = do
    mAge <- cache <$> asks appConfig
    case mAge of
        Nothing    -> pure []
        (Just age) -> (fst <$>) <$> load age
