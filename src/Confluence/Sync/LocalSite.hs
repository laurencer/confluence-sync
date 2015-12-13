{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Confluence.Sync.LocalSite where

import           Control.Applicative

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Utils (replace)
import           Data.Tree
import           Data.Tree.Zipper

import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI

import           System.Directory
import           System.FilePath

import           Confluence.Sync.XmlRpc.Types

{-

This file contains the core data structures of the application:

- SiteFile      -> SiteTree       -> SiteFileZipper
- PageReference -> PageTree       -> PageZipper
- SyncReference -> ConfluenceTree -> ConfluenceZipper

Site File
=========

A site file represents a local file that exists within the directory being
synced. The reason for the data structure is to materialise whether it is a
directory (which would otherwise require an IO action) and to capture a
consistent snapshot of the filesystem (because otherwise local mutations could
introduce a class of errors that would not otherwise be present - and instead we
simply fail when accessing a file rather than having an "incorrect" result).

The site tree simply represents all the files and directories in the sync root.

Page Reference
==============

Not every file in the directory generates a page, and not every page corresponds
to a single file. For example, assets, such as images do not result in new
Confluence pages being created.

Similarly, directories, whilst not having content
themselves may instead be "replaced" with a README.md or README.html that is at
the root of the directory. E.g. for /foo - a Confluence page will be created where /foo is but with the
content from /foo/README.md.

As a result, every page reference has a reference to its actual site file - which
represents where it should lie in the hierarchy of pages, and a reference to where
it should actually get its content from.

In the event a directory uses a file's contents - then instead of creating the file,
a shadow reference is generated instead. This allows us to detect that the page's
contents have already been rendered and we only render the page once. For example,
if we have /foo from the example above (with content from /foo/README.md), we don't
also want to separately render the same content at /foo/README.md.

The shadow reference is useful however for resolving links and other paths because we
can look it up based on the page reference (e.g. a link to /foo/README.html can be
redirected to /foo based on the shadow reference).

Sync Reference
==============

A sync reference pairs an actual page on Confluence (e.g. with its id, url and
related data) with a Page Reference. Once we have both of these, we can resolve
links between pages and provide more of the advanced functionality.

Only actual page references (not shadow references) have a sync reference entry.

Zippers
=======

All of the aforementioned data structures form trees logically - whether its
Confluence page hierarchies or folders/files on the file system.

The reason zippers are used is to allow lookups and references based on the
position of a page or file in the hierarchy.

For example, it allows a page to quickly ascertain the Confluence page id of its
parent to ensure that it is positioned correctly. Similarly, it allows for relative
lookups based on links (e.g. ../bar.img can resolve based on the parents location).

-}

-------------------------------------------------------------------------------
-- Site Tree.
-------------------------------------------------------------------------------

-- | Tree of all the local files that comprise the site to sync.
type SiteTree = Tree SiteFile

type SiteFileZipper = TreePos Full SiteFile

data SiteFile = SiteFile {
  -- Absolute path to the location.
  filePath    :: FilePath
, isDirectory :: Bool
} deriving (Eq, Ord, Show)

-- | Builds a site tree from the root element.
buildSiteTree :: FilePath -> IO SiteTree
buildSiteTree root = do
  unfoldTreeM buildTree root
  where listDirectory :: FilePath -> IO ([ FilePath ])
        listDirectory dir = do
          relativeContents    <- (filter (`notElem` [".", ".."])) <$> (getDirectoryContents dir)
          let withoutHidden    = filter (\p -> not $ "." `isPrefixOf` p) relativeContents
          let absoluteContents = map (dir </>) withoutHidden
          return absoluteContents
        buildTree :: FilePath -> IO (SiteFile, [ FilePath ])
        buildTree filePath = do
          isDirectory <- doesDirectoryExist filePath
          children    <- if (isDirectory) then listDirectory filePath else return []
          return $ (SiteFile { .. }, children)

--  | True if the file represents content that can be converted to a Confluence page.
isSiteFileAPage :: SiteFile -> Bool
isSiteFileAPage SiteFile { filePath } = Set.member (CI.mk . takeExtension $ filePath) lowercasePageExtensions

-- | List of valid extensions for pages (all lower-case).
lowercasePageExtensions = Set.fromList $ map CI.mk [ ".html", ".htm", ".md", ".markdown" ]

-------------------------------------------------------------------------------
-- Page Tree.
-------------------------------------------------------------------------------

-- | Tree of the pages that need to exist in Confluence.
type PageTree = Tree PageReference

-- | Zipper focused at a page.
type PageZipper = TreePos Full PageReference

data PageReference = PageReference {
  -- Where this is actually rooted.
  sitePosition'    :: SiteFileZipper
  -- Where the contents of this page are located.
  -- This can differ from the site position in the case of
  -- a directory (e.g. /foo and /foo/README.md are merged to become /foo).
  -- Where this occurs a shadow reference is created in its place.
, pageSource'      :: Maybe SiteFileZipper
-- A shadow reference exists where the page is actually merged with another
-- (but the reference is included to allow easy lookups).
} | ShadowReference PageReference
  deriving (Eq, Show)

instance Ord PageZipper where
  p1 `compare` p2 = (label . sitePosition . label $ p1) `compare` (label . sitePosition . label $ p2)

notShadowReference :: PageReference -> Bool
notShadowReference (ShadowReference _) = False
notShadowReference (PageReference _ _) = True

sitePosition :: PageReference -> SiteFileZipper
sitePosition PageReference { sitePosition' } = sitePosition'
sitePosition (ShadowReference ref) = fromJust $ pageSource' ref

pageSource :: PageReference -> Maybe SiteFileZipper
pageSource PageReference { pageSource' } = pageSource'
pageSource (ShadowReference ref) = pageSource' ref

potentialPageNames :: String -> PageZipper -> [String]
potentialPageNames syncTitle page =
  if (isRoot page)
    then [syncTitle] -- Always return just the sync title for the root page.
    else [
      title                                   -- Just the raw page title.
    , title ++ " (" ++ syncTitle ++ ")"       -- Raw page title and sync title.
    , title ++ " (" ++ uniqueTitlePath ++ ")" -- Completely unique path.
    ]
  where pagePath = filePath . label . sitePosition . label $ page
        title = prettifyPageName . takeBaseName $ pagePath
        rootPath = filePath . label . sitePosition . label . root $ page
        pathElements = map prettifyPageName $ filter (/= ".") (splitDirectories $ makeRelative rootPath (takeDirectory pagePath))
        uniqueTitlePath = if (null pathElements) then syncTitle else (intercalate " / " pathElements) ++ " - " ++ syncTitle

-- If the PageReference is Just - it means the current page is actually a shadow reference to another page.
mkPageReference :: (SiteFileZipper, Maybe PageReference) -> (PageReference, [ (SiteFileZipper, Maybe PageReference) ])
mkPageReference (zipper, shadowReference) =
  case shadowReference of
    Just reference -> (ShadowReference reference, [])
    Nothing ->
      let siteFile        = label zipper
          isDirectory'    = isDirectory siteFile
          -- Content pages are naturally confluence pages (e.g. markdown or HTML)
          -- A directory is also a page IFF it has actual content pages below it.
          -- We exclude any directories that have no descendant pages.
          isZipperAPage p = (isSiteFileAPage . label $ p) || (isDirectory' && zipperHasDescendantPages p)
          zipperHasDescendantPages = any (isSiteFileAPage . label) . traverseZipper
          pageChildren    = filter isZipperAPage (traverseChildren zipper)
          pageSource      = if (isDirectory') then (findDirectoryPage pageChildren) else (Just zipper)
          reference       = PageReference zipper pageSource
          pageChildren'   = map (\c -> (c, if ((Just c) == pageSource) then (Just reference) else Nothing)) pageChildren
      in (reference, pageChildren')

findDirectoryPage :: [ SiteFileZipper ] -> Maybe SiteFileZipper
findDirectoryPage zippers = do
  (check "readme") <|> (check "index")
  where
        zipperFilename = takeBaseName . filePath . label
        check fileName = find ((== fileName) . CI.mk . zipperFilename) zippers

-- | Builds a site tree from the root element.
buildPageTree :: SiteTree -> PageTree
buildPageTree siteTree =
  unfoldTree buildTree (rootZipper, Nothing)
  where rootZipper = fromTree siteTree
        buildTree :: (SiteFileZipper, Maybe PageReference) -> (PageReference, [ (SiteFileZipper, Maybe PageReference) ])
        buildTree = mkPageReference

traverseChildren :: TreePos Full a -> [ TreePos Full a ]
traverseChildren zipper =
  maybe [] traverseSiblings (firstChild zipper)
  where traverseSiblings :: TreePos Full b -> [ TreePos Full b ]
        traverseSiblings z = case (next z) of
          Just continue -> z : (traverseSiblings continue)
          Nothing       -> [ z ]

traverseZipper :: TreePos Full a -> [ TreePos Full a ]
traverseZipper zipper =
  zipper : ((traverseChildren zipper) >>= traverseZipper)

-------------------------------------------------------------------------------
-- Page/Element Functions.
-------------------------------------------------------------------------------

-- | Takes the filename of a page and sanitizes it for a Confluence page name.
--   Basically removes common symbols and capitalises each word.
prettifyPageName :: String -> String
prettifyPageName path = (unwords . map capitaliseWord . words) $ (foldr replaceCharacter (takeBaseName path) [
      ("-", " ")
    , ("_", " ")
  ])
  where replaceCharacter :: (String, String) -> String -> String
        replaceCharacter (needle, replaced) str = replace needle replaced str
        capitaliseWord :: String -> String
        capitaliseWord [] = []
        capitaliseWord (c : cs) = toUpper c : map toLower cs
-------------------------------------------------------------------------------
-- Tree of Local and Remote Pages
-------------------------------------------------------------------------------

type ConfluenceTree = Tree SyncReference

type ConfluenceZipper = TreePos Full SyncReference

data SyncReference = SyncReference {
  pagePosition :: PageReference
, remotePage   :: PageSummary
} deriving (Show, Eq)

pageTreeToSyncTree :: PageZipper -> (PageZipper -> PageSummary) -> ConfluenceTree
pageTreeToSyncTree pageTree localToRemote =
  unfoldTree unfold pageTree
  where unfold :: PageZipper -> (SyncReference, [ PageZipper ])
        unfold zipper = ((SyncReference (label zipper) (localToRemote zipper)), (traverseChildren zipper))
