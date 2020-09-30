{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Brok.IO.CLI where

import RIO
import qualified RIO.Text as T 
import Brok.Types.Brok     (Brok, appConfig)
import Brok.Types.Config   (noColor)
import Data.Text.IO        (hPutStr, hPutStrLn, putStrLn)
import System.Console.ANSI (Color (Blue, Green, Magenta, Red, Yellow), ColorIntensity (Dull),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), hClearLine,
                            hCursorUpLine, hSetSGR)

setSGR :: Handle -> [SGR] -> Brok ()
setSGR hndl settings = do
    colourize <- not . noColor <$> asks appConfig
    when colourize $ liftIO (hSetSGR hndl settings)

blank :: Brok ()
blank = liftIO $ putStrLn ""

message :: Text -> Brok ()
message msg = do
    setSGR stdout [SetColor Foreground Dull Blue]
    liftIO $ putStrLn msg
    setSGR stdout [Reset]

mehssage :: Text -> Brok ()
mehssage msg = do
    setSGR stdout [SetColor Foreground Dull Yellow]
    liftIO $ putStrLn msg
    setSGR stdout [Reset]

header :: Text -> Brok ()
header msg = do
    setSGR stdout [SetColor Foreground Dull Magenta]
    liftIO $ putStrLn $ "*** " <> msg <> " ***"
    setSGR stdout [Reset]

successMessage :: Text -> Brok ()
successMessage msg = do
    setSGR stdout [SetColor Foreground Dull Green]
    liftIO $ putStrLn msg
    setSGR stdout [Reset]

errorMessage :: Text -> Brok ()
errorMessage msg = do
    setSGR stderr [SetColor Foreground Dull Red]
    liftIO $ hPutStrLn stderr msg
    setSGR stderr [Reset]

errors :: Text -> [Text] -> Brok ()
errors _ [] = pure ()
errors msg missing = do
    errorMessage msg
    liftIO $ hPutStrLn stderr ""
    errorMessage (T.unlines $ ("- " <>) <$> missing)

split :: Handle -> Color -> Text -> Text -> Brok ()
split hdl color left right = do
    setSGR hdl [SetColor Foreground Dull color]
    liftIO $ hPutStr hdl left
    setSGR hdl [Reset]
    liftIO $ hPutStr hdl $ ": " <> right
    liftIO $ hPutStrLn hdl ""

splitErr :: Text -> Text -> Brok ()
splitErr = split stderr Red

splitOut :: Text -> Text -> Brok ()
splitOut = split stdout Green

splitMeh :: Text -> Text -> Brok ()
splitMeh = split stdout Yellow

replace :: Text -> Brok ()
replace msg = liftIO $ do
    hCursorUpLine stdout 1
    hClearLine stdout
    putStrLn msg
