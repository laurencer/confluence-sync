{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Confluence.Sync.LocalFiles (
  FoundFile(..)
, LocalAttachment(..)
, isPage
, findFilesInDirectory
, friendlyName
, prettyName
, getPageContents
, getAttachments
, localAttachmentRemoteName
) where

import           Control.Monad (forM)

import           Crypto.Hash
import           CMark

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC

import           Data.Char
import           Data.String.Utils
import           Data.List (intercalate, isPrefixOf, find, elem)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Text.XML.HXT.Core
import           Text.HandsomeSoup
import           Text.Heredoc

import           System.Directory (doesDirectoryExist, getDirectoryContents)
import           System.FilePath

import           Confluence.Sync.XmlRpc.Types

-------------------------------------------------------------------------------
-- General Local File Handling.
-------------------------------------------------------------------------------

data FoundFile = FoundFile {
  -- File name (including extension).
  name        :: String
  -- Full path to the file.
, fullPath    :: FilePath
  -- The path to this directory relative to the root of the site.
, directories :: [ String ]
  -- Root of the sync directory.
, rootPath    :: FilePath
} deriving Show

isPage :: FoundFile -> Bool
isPage f = (isMarkdownPage f) || (isHtmlPage f)

isMarkdownPage :: FoundFile -> Bool
isMarkdownPage file = Set.member (map toLower (takeExtension (name file))) markdownExtensions
  where markdownExtensions = Set.fromList [ ".md", ".markdown" ]

isHtmlPage :: FoundFile -> Bool
isHtmlPage file = Set.member (map toLower (takeExtension (name file))) htmlExtensions
  where htmlExtensions = Set.fromList [ ".html", ".htm" ]

friendlyName :: FoundFile -> String
friendlyName file = prettyName $ dropExtension (name file)

-- Takes a filename and makes into something which is nice for page titles.
-- This involves replacing special characters and capitalising each word.
prettyName :: String -> String
prettyName string = 
  (unwords . map capitaliseWord . words) $ (foldr replaceCharacter string [
      ("-", " ")
    , ("_", " ")
  ])
  where replaceCharacter :: (String, String) -> String -> String
        replaceCharacter (needle, replaced) str = replace needle replaced str
        capitaliseWord :: String -> String
        capitaliseWord [] = []
        capitaliseWord (c : cs) = toUpper c : map toLower cs

-------------------------------------------------------------------------------
-- Local Attachment Handling.
-------------------------------------------------------------------------------

data LocalAttachment = LocalAttachment {
  -- Filename of the attachment.
  localAttachmentName  :: String
  -- How it is referenced in the document (e.g. what path is used to refer to it)
, localReference       :: String
  -- Actual path to file contents on disk.
, localAttachmentPath  :: FilePath
  -- Path relative to the sync root.
, attachmentRelativePath  :: FilePath
}

-- Generates the filename of the attachment in Confluence
localAttachmentRemoteName :: LocalAttachment -> IO String
localAttachmentRemoteName local = do
  hash <- attachmentHash local
  let relative = attachmentRelativePath local
  let extension = takeExtension relative
  let baseName = dropExtension relative
  let safeName = replace "/" "_" (replace "_" "__" baseName)
  return $ safeName ++ "_" ++ hash ++ extension

attachmentHash :: LocalAttachment -> IO String
attachmentHash LocalAttachment { localAttachmentPath } = do
  contents <- LBS.readFile localAttachmentPath
  return . BSC.unpack $ digestToHexByteString $ md5 contents
  where md5 :: LBS.ByteString -> Digest MD5
        md5 = hashlazy

getAttachments :: FoundFile -> IO [ LocalAttachment ]
getAttachments (file@FoundFile { fullPath, rootPath }) = do
  pageContents <- getPageAsRawHtml file
  references   <- findLocalAttachments pageContents
  let all = (flip fmap) references $ (\path ->
              let pathToAttachment = if (isPrefixOf "/" path) 
                                      then (rootPath </> (tail path))
                                      else ((takeDirectory fullPath) </> path)
              in LocalAttachment (takeFileName path) path pathToAttachment (makeRelative rootPath pathToAttachment)
            )
  let validExtensions = Set.fromList [ ".pdf", ".jpeg", ".jpg", ".png", ".js", ".css" ]
      onlyValidExtensions LocalAttachment { localAttachmentName } = Set.member (map toLower (takeExtension (localAttachmentName))) validExtensions
  return $ filter onlyValidExtensions all

-------------------------------------------------------------------------------
-- Helper functions for rewriting site urls.
-------------------------------------------------------------------------------

getPageAsRawHtml :: FoundFile -> IO T.Text
getPageAsRawHtml (file@FoundFile { fullPath }) =
  if (isHtmlPage file) then (TIO.readFile fullPath)
    else if (isMarkdownPage file) then (\html -> T.append githubCss html) <$> (commonmarkToHtml [optSafe, optSmart]) <$> (TIO.readFile fullPath)
    else return ""

getPageContents :: FoundFile -> [ (LocalAttachment, Attachment) ] -> [ (FoundFile, PageSummary) ] -> IO String
getPageContents (file@FoundFile { fullPath }) attachmentMapping pageMapping = 
  if (isHtmlPage file) then htmlContent
    else if (isMarkdownPage file) then markdownContent
    else return ""
  where htmlContent     = wrapHtml <$> ((getPageAsRawHtml file) >>= processAttachments >>= processLinks)
        markdownContent = wrapHtml <$> ((getPageAsRawHtml file) >>= processAttachments >>= processLinks)
        processLinks = rewriteLinks pageMapping file
        processAttachments = rewriteAttachmentReferences referenceToUrl
          where referenceToUrl = Map.fromList $ fmap (\(l, r) -> ((localReference l), (attachmentUrl r))) attachmentMapping
        wrapHtml :: String -> String
        wrapHtml contents = concat [ prefix, contents, suffix ]
          where prefix = "<ac:macro ac:name='html'><ac:plain-text-body><![CDATA["
                suffix = "]]></ac:plain-text-body></ac:macro>"
          
-------------------------------------------------------------------------------
-- Helper functions for rewriting site urls.
-------------------------------------------------------------------------------

type SiteReference = String
type AttachmentUrl = String
type LinkedPageUrl = String

lowerCase str = map toLower str
notDataUri uri = not (isPrefixOf "data:" (lowerCase uri))
notExternalUri uri = not ((isPrefixOf "http:" (lowerCase uri)) || (isPrefixOf "https:" (lowerCase uri)))
isSiteUri uri = (notDataUri uri) && (notExternalUri uri)
modifyAttribute name f = changeAttrValue f `when` hasName name

-------------------------------------------------------------------------------
-- Link rewriting
-------------------------------------------------------------------------------

rewriteLinks :: [ (FoundFile, PageSummary) ] -> FoundFile -> String -> IO String
rewriteLinks pageMapping file html =
  let doc = readString [withParseHTML yes, withWarnings no] $ html
      isSiteLink = isElem >>> hasName "a" >>> hasAttr "href" >>> getAttrValue "href" >>> isA isSiteUri
      findPage = fromLinkToPage pageMapping file
      updateReference reference = maybe reference id (findPage reference)
      replaceLinks = processAttrl (modifyAttribute "href" updateReference) `when` isSiteLink
      parseAndReplace = runX $ doc >>> 
        processTopDown replaceLinks
        >>> writeDocumentToString [ withOutputEncoding isoLatin1 ]
  in concat <$> parseAndReplace

fromLinkToPage :: [ (FoundFile, PageSummary) ] -> FoundFile -> SiteReference -> Maybe LinkedPageUrl
fromLinkToPage pageMapping currentFile reference =
  let relativeToRootReference = if (isPrefixOf "/" reference) 
        then dropTrailingPathSeparator $ (tail reference)
        else dropTrailingPathSeparator $ (joinPath $ (directories currentFile) ++ [ reference ])
      searchPaths = if (hasExtension relativeToRootReference) 
        then [ relativeToRootReference ]
        else [ (relativeToRootReference </> "index.html"), (relativeToRootReference </> "index.htm") ]
      normalizedSearchPaths = fmap lowerCase searchPaths
      normalizedRelativeToRoot file = lowerCase $ makeRelative (rootPath file) (fullPath file)
      maybeFileAndSummary = find (\(ff, ps) -> (normalizedRelativeToRoot ff) `elem` normalizedSearchPaths) pageMapping
  in (\(ff, ps) -> pageSummaryUrl ps) <$> maybeFileAndSummary


-------------------------------------------------------------------------------
-- Asset parsing/replacing (with attachments)
-------------------------------------------------------------------------------

-- | Finds all files referenced in the page that need to be attached to it.
--   For example, this includes images and PDFs.
--
-- >>> findLocalAttachments $ T.pack "<body><a href='/foo.pdf'>My Link</a><img src='my_fancy_pic.png' /></body>"
-- ["/foo.pdf","my_fancy_pic.png"]
--
-- >>> findLocalAttachments $ T.pack "<body><img src='data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO9TXL0Y4OHwAAAABJRU5ErkJggg==' alt='Red dot' /></body>"
-- []
--
-- >>> findLocalAttachments $ T.pack "<body><a href='HTTP://www.google.com/foo.pdf'>My Link</a><img src='https://imgur.com/my_fancy_pic.png' /></body>"
-- []
--
findLocalAttachments :: T.Text -> IO [FilePath]
findLocalAttachments html = do
  let doc = readString [withParseHTML yes, withWarnings no] $ T.unpack html
  allReferences <- runX $ doc >>> (css "a" ! "href") <+> (css "img" ! "src") <+> (css "link" ! "href")
  return $ filter isSiteUri allReferences

-- Rewrites references to images to point to the attachment urls.
rewriteAttachmentReferences :: (Map SiteReference AttachmentUrl) -> T.Text -> IO String
rewriteAttachmentReferences referenceToUrl html =
  let doc = readString [withParseHTML yes, withWarnings no] $ T.unpack html
      isSiteLink = isElem >>> hasName "a" >>> hasAttr "href" >>> getAttrValue "href" >>> isA isSiteUri
      isSiteImage = isElem >>> hasName "img" >>> hasAttr "src" >>> getAttrValue "src" >>> isA isSiteUri
      isSiteLinkedResource = isElem >>> hasName "link" >>> hasAttr "href" >>> getAttrValue "href" >>> isA isSiteUri
      isSiteScript = isElem >>> hasName "script" >>> hasAttr "src" >>> getAttrValue "src" >>> isA isSiteUri
      updateReference existing = Map.findWithDefault existing existing referenceToUrl
      replaceLinks = processAttrl (modifyAttribute "href" updateReference) `when` isSiteLink
      replaceLinkedResources = processAttrl (modifyAttribute "href" updateReference) `when` isSiteLinkedResource
      replaceImages = processAttrl (modifyAttribute "src" updateReference) `when` isSiteImage
      replaceScripts = processAttrl (modifyAttribute "src" updateReference) `when` isSiteScript
      parseAndReplace = runX $ doc >>> 
        (processTopDown replaceLinks) 
        >>> (processTopDown replaceImages)
        >>> (processTopDown replaceLinkedResources)
        >>> (processTopDown replaceScripts)
        >>> writeDocumentToString [ withOutputEncoding isoLatin1 ]
  in concat <$> parseAndReplace

-------------------------------------------------------------------------------
-- Find files to sync.
-------------------------------------------------------------------------------

-- Recurses through a directory and finds files to sync.
findFilesInDirectory :: FilePath -> IO [FoundFile]
findFilesInDirectory root = 
  recurse [] root
  where recurse directories current = do
          -- Get contents of the directory
          names <- getDirectoryContents current
          -- Only select actual files/folders
          let properNames = filter (`notElem` [".", ".."]) names
          -- Iterate through the list of files.
          found <- forM properNames $ \name -> do
            -- Construct the full path to the file.
            let fullPath = current </> name
            -- Check whether it is a directory.
            isDirectory <- doesDirectoryExist fullPath
            if isDirectory
              -- Recurse on any directory we find (add the directory to the hirearchy.)
              then recurse (name : directories) fullPath
              else return [ (FoundFile name fullPath (reverse directories) root) ]
          return (concat found)

-------------------------------------------------------------------------------
-- GitHub CSS Style.
-------------------------------------------------------------------------------

githubCss = [here|
<style>
body {
  font-family: Helvetica, arial, sans-serif;
  font-size: 14px;
  line-height: 1.6;
  padding-top: 10px;
  padding-bottom: 10px;
  background-color: white;
  padding: 30px;
  color: #333;
}

body > *:first-child {
  margin-top: 0 !important;
}

body > *:last-child {
  margin-bottom: 0 !important;
}

a {
  color: #4183C4;
  text-decoration: none;
}

a.absent {
  color: #cc0000;
}

a.anchor {
  display: block;
  padding-left: 30px;
  margin-left: -30px;
  cursor: pointer;
  position: absolute;
  top: 0;
  left: 0;
  bottom: 0;
}

h1, h2, h3, h4, h5, h6 {
  margin: 20px 0 10px;
  padding: 0;
  font-weight: bold;
  -webkit-font-smoothing: antialiased;
  cursor: text;
  position: relative;
}

h2:first-child, h1:first-child, h1:first-child + h2, h3:first-child, h4:first-child, h5:first-child, h6:first-child {
  margin-top: 0;
  padding-top: 0;
}

h1:hover a.anchor, h2:hover a.anchor, h3:hover a.anchor, h4:hover a.anchor, h5:hover a.anchor, h6:hover a.anchor {
  text-decoration: none;
}

h1 tt, h1 code {
  font-size: inherit;
}

h2 tt, h2 code {
  font-size: inherit;
}

h3 tt, h3 code {
  font-size: inherit;
}

h4 tt, h4 code {
  font-size: inherit;
}

h5 tt, h5 code {
  font-size: inherit;
}

h6 tt, h6 code {
  font-size: inherit;
}

h1 {
  font-size: 28px;
  color: black;
}

h2 {
  font-size: 24px;
  border-bottom: 1px solid #cccccc;
  color: black;
}

h3 {
  font-size: 18px;
}

h4 {
  font-size: 16px;
}

h5 {
  font-size: 14px;
}

h6 {
  color: #777777;
  font-size: 14px;
}

p, blockquote, ul, ol, dl, li, table, pre {
  margin: 15px 0;
}

hr {
  border: 0 none;
  color: #cccccc;
  height: 4px;
  padding: 0;
}

body > h2:first-child {
  margin-top: 0;
  padding-top: 0;
}

body > h1:first-child {
  margin-top: 0;
  padding-top: 0;
}

body > h1:first-child + h2 {
  margin-top: 0;
  padding-top: 0;
}

body > h3:first-child, body > h4:first-child, body > h5:first-child, body > h6:first-child {
  margin-top: 0;
  padding-top: 0;
}

a:first-child h1, a:first-child h2, a:first-child h3, a:first-child h4, a:first-child h5, a:first-child h6 {
  margin-top: 0;
  padding-top: 0;
}

h1 p, h2 p, h3 p, h4 p, h5 p, h6 p {
  margin-top: 0;
}

li p.first {
  display: inline-block;
}

ul, ol {
  padding-left: 30px;
}

ul :first-child, ol :first-child {
  margin-top: 0;
}

ul :last-child, ol :last-child {
  margin-bottom: 0;
}

dl {
  padding: 0;
}

dl dt {
  font-size: 14px;
  font-weight: bold;
  font-style: italic;
  padding: 0;
  margin: 15px 0 5px;
}

dl dt:first-child {
  padding: 0;
}

dl dt > :first-child {
  margin-top: 0;
}

dl dt > :last-child {
  margin-bottom: 0;
}

dl dd {
  margin: 0 0 15px;
  padding: 0 15px;
}

dl dd > :first-child {
  margin-top: 0;
}

dl dd > :last-child {
  margin-bottom: 0;
}

blockquote {
  border-left: 4px solid #dddddd;
  padding: 0 15px;
  color: #777777;
}

blockquote > :first-child {
  margin-top: 0;
}

blockquote > :last-child {
  margin-bottom: 0;
}

table {
  padding: 0;
}
table tr {
  border-top: 1px solid #cccccc;
  background-color: white;
  margin: 0;
  padding: 0;
}

table tr:nth-child(2n) {
  background-color: #f8f8f8;
}

table tr th {
  font-weight: bold;
  border: 1px solid #cccccc;
  text-align: left;
  margin: 0;
  padding: 6px 13px;
}

table tr td {
  border: 1px solid #cccccc;
  text-align: left;
  margin: 0;
  padding: 6px 13px;
}

table tr th :first-child, table tr td :first-child {
  margin-top: 0;
}

table tr th :last-child, table tr td :last-child {
  margin-bottom: 0;
}

img {
  max-width: 100%;
}

span.frame {
  display: block;
  overflow: hidden;
}

span.frame > span {
  border: 1px solid #dddddd;
  display: block;
  float: left;
  overflow: hidden;
  margin: 13px 0 0;
  padding: 7px;
  width: auto;
}

span.frame span img {
  display: block;
  float: left;
}

span.frame span span {
  clear: both;
  color: #333333;
  display: block;
  padding: 5px 0 0;
}

span.align-center {
  display: block;
  overflow: hidden;
  clear: both;
}

span.align-center > span {
  display: block;
  overflow: hidden;
  margin: 13px auto 0;
  text-align: center;
}

span.align-center span img {
  margin: 0 auto;
  text-align: center;
}

span.align-right {
  display: block;
  overflow: hidden;
  clear: both;
}

span.align-right > span {
  display: block;
  overflow: hidden;
  margin: 13px 0 0;
  text-align: right;
}

span.align-right span img {
  margin: 0;
  text-align: right;
}

span.float-left {
  display: block;
  margin-right: 13px;
  overflow: hidden;
  float: left;
}

span.float-left span {
  margin: 13px 0 0;
}

span.float-right {
  display: block;
  margin-left: 13px;
  overflow: hidden;
  float: right;
}

span.float-right > span {
  display: block;
  overflow: hidden;
  margin: 13px auto 0;
  text-align: right;
}

code, tt {
  margin: 0 2px;
  padding: 0 5px;
  white-space: nowrap;
  border: 1px solid #eaeaea;
  background-color: #f8f8f8;
  border-radius: 3px;
}

pre code {
  margin: 0;
  padding: 0;
  white-space: pre;
  border: none;
  background: transparent;
}

.highlight pre {
  background-color: #f8f8f8;
  border: 1px solid #cccccc;
  font-size: 13px;
  line-height: 19px;
  overflow: auto;
  padding: 6px 10px;
  border-radius: 3px;
}

pre {
  background-color: #f8f8f8;
  border: 1px solid #cccccc;
  font-size: 13px;
  line-height: 19px;
  overflow: auto;
  padding: 6px 10px;
  border-radius: 3px;
}

pre code, pre tt {
  background-color: transparent;
  border: none;
}
</style>
|]