{-# LANGUAGE NoImplicitPrelude #-}

module Brok.Types.Document where

import RIO
import Brok.IO.Http      (check)
import Brok.Parser.Links (links)
import Brok.Types.Brok   (Brok)
import Brok.Types.Link
import Brok.Types.URL    (URL)
import Data.Map.Strict   as M (empty, insert, (!?))

type TFilePath = FilePath

type Error = Text

data Phase
    = Content Text
    | NotFound
    | ParseError Error
    | Links [Link]
    deriving (Show, Eq)

data Document =
    Document TFilePath
             Phase
    deriving (Show, Eq)

type LDAcc a = (LinkDictionary, [a])

getPath :: Document -> TFilePath
getPath (Document path _) = path

notFound :: TFilePath -> Document
notFound path = Document path NotFound

withContent :: TFilePath -> Text -> Document
withContent path text = Document path (Content text)

parseLinks :: Document -> Document
parseLinks (Document path (Content text)) =
    case links text of
        Left err   -> Document path (ParseError err)
        Right lnks -> Document path (Links $ urlToLink <$> lnks)
parseLinks result = result

findLinks :: ([URL] -> Link -> Link) -> [URL] -> Document -> Document
findLinks fn urls (Document path (Links lnks)) = Document path (Links $ fn urls <$> lnks)
findLinks _ _ result                           = result

cachedLinks :: [URL] -> Document -> Document
cachedLinks = findLinks cachedLink

ignoredLinks :: [URL] -> Document -> Document
ignoredLinks = findLinks ignoredLink

-- go over each link, building up a list of found links
checkLink :: LDAcc Link -> Link -> Brok (LDAcc Link)
checkLink (prev, lnks) (Link url UnresolvedLink) = do
    case prev !? url of
        Just result -> pure (prev, lnks <> [result])
        Nothing -> do
            result <- check (Link url UnresolvedLink)
            let newPrev = insert url result prev
            pure (newPrev, lnks <> [result])
checkLink (prev, lnks) lnk = pure (prev, lnks <> [lnk])

-- go over each document, building up a list of found links
checkDocument :: LDAcc Document -> Document -> Brok (LDAcc Document)
checkDocument (prev, documents) (Document path (Links lnks)) = do
    (newPrev, newLnks) <- foldM checkLink (prev, []) lnks
    pure (newPrev, documents <> [Document path (Links newLnks)])
checkDocument (prev, documents) document = pure (prev, documents <> [document])

-- check links in all documents
checkLinks :: [Document] -> Brok [Document]
checkLinks documents = snd <$> foldM checkDocument (M.empty, []) documents

justLinks :: Document -> [Link]
justLinks (Document _ (Links lnks)) = lnks
justLinks _                         = []
