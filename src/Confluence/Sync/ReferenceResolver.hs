{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Confluence.Sync.ReferenceResolver where

import           Control.Monad (forM, filterM)

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String.Utils (replace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Tree
import           Data.Tree.Zipper

import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI

import qualified Network.URL as URL

import           Safe (tailMay)

import           System.Directory
import           System.FilePath

import           Confluence.Sync.LocalSite
import           Confluence.Sync.XmlRpc.Types

-- | Resolves an internal site link to the actual page.
--   This lets us rewrite the internal links to Confluence references.
resolvePageLink :: ConfluenceZipper -> FilePath -> Maybe ConfluenceZipper
resolvePageLink current link = do
  let rootPath        = filePath . label . sitePosition . pagePosition . label . root $ current
  currentFilePath <- (filePath . label) <$> (pageSource . pagePosition . label $ current)
  normalized      <- calculateAbsolutePath rootPath currentFilePath (decodeUrl link)
  let pathToComponents = (map (CI.mk)) . splitDirectories
      getPathToZipper  = filePath . label . sitePosition . pagePosition
      isParentOf z = (pathToComponents . getPathToZipper $ z) `isPrefixOf` (pathToComponents normalized)
      -- Compare without the extension (because it may change and is usually irrelevant).
      isEqualTo z = (CI.mk . dropExtension . getPathToZipper $ z) == (CI.mk . dropExtension $ normalized)
  -- This has resolved the page BUT may be pointing at a shadow reference.
  page <- findInZipper' isEqualTo (root current)
  -- We need to resolve the shadow reference.
  resolvedPage <- case (pagePosition . label $ page) of
    (PageReference _ _)   -> return page
    (ShadowReference ref) -> find (\z -> (pagePosition . label $ z) == ref) (traverseZipper (root current))
  return resolvedPage

-- | Resolves an attachment to page - which lets us rewrite the link.
resolvePageAttachment :: ConfluenceZipper -> FilePath -> Maybe SiteFileZipper
resolvePageAttachment current link = do
  let rootNode        = sitePosition . pagePosition . label . root $ current
      rootPath        = filePath . label . sitePosition . pagePosition . label . root $ current
  currentFilePath <- (filePath . label) <$> (pageSource . pagePosition . label $ current)
  normalized      <- calculateAbsolutePath rootPath currentFilePath (decodeUrl link)
  let pathToComponents = (map (CI.mk)) . splitDirectories
      isParentOf z = (pathToComponents . filePath $ z) `isPrefixOf` (pathToComponents normalized)
      isEqualTo z = ((CI.mk . filePath $ z) == (CI.mk $ normalized))
  resolved <- findInZipper' isEqualTo rootNode
  return resolved

-- | Decodes any URL-encoded (% encoded).
decodeUrl :: String -> String
decodeUrl reference = maybe reference id (URL.decString False reference)

-- | Removes any indirection in the path (e.g. `..` or `.` ) using
--   simple pattern matching.
--
-- >>> calculateAbsolutePath "/site/src" "/site/src/category/index.html" "/about/index"
-- Just "/site/src/about/index"
--
-- >>> calculateAbsolutePath "/site/src" "/site/src/category/index" "../image.png"
-- Just "/site/src/image.png"
--
calculateAbsolutePath :: FilePath -> FilePath -> String -> Maybe FilePath
calculateAbsolutePath rootPath referencingFilePath link = do
  let currentFileDirectory = dropFileName referencingFilePath
      absoluteLink = if ([pathSeparator] `isPrefixOf` link)
        then rootPath </> (tail link)
        else currentFileDirectory </> link
  dropTrailingPathSeparator <$> (normalizePath rootPath absoluteLink)

-- Traverses the Zipper in a DFS manner - selecting the path by choosing element at each elvel
-- which is still a parent of the target.
findInZipper :: (a -> Bool) -> (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findInZipper isParentOf isEqualTo zipper =
  if (isEqualTo . label $ zipper) 
    then Just zipper
    else (find (isParentOf . label) (traverseChildren zipper)) >>= (findInZipper isParentOf isEqualTo)

findInZipper' :: (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findInZipper' predicate zipper =
  find (predicate . label) nodes
  where nodes = (traverseZipper zipper)

-- | Removes any indirection in the path (e.g. `..` or `.` ) using
--   simple pattern matching.
--
-- >>> normalizePath "/bar/fizz" "foo/bar/../baz/"
-- Just "/bar/fizz/foo/baz"
--
-- >>> normalizePath "/bar/fizz" "./foo/./bar/../baz"
-- Just "/bar/fizz/foo/baz"
--
-- >>> normalizePath "/bar/fizz" "./foo/./bar//../baz"
-- Just "/bar/fizz/foo/baz"
--
-- >>> normalizePath "/bar/fizz" "../baz"
-- Nothing
--
-- >>> normalizePath "/foo/bar" "/foo/bar/../baz"
-- Nothing
--
normalizePath :: FilePath -> FilePath -> Maybe FilePath
normalizePath root path = do
  resolvedComponents <- reverse <$> (resolveParentLinks (reverse withoutEmpties))
  return $ dropTrailingPathSeparator $ root </> (foldl' (</>) "" resolvedComponents)
  where components = splitPath (makeRelative root path)
        -- Filter out redirections to the current directory.
        withoutDot = filter (/= "./") components
        withoutEmpties = filter (/= "") withoutDot
        -- | Resolves parent indirections in the folder structure.
        --   If it goes outside the root of the directory, it returns Nothing.
        --   Expects the list to be reversed! (e.g. /foo/bar/baz == [baz, bar/, foo/])
        resolveParentLinks :: [ FilePath ] -> Maybe ([ FilePath ])
        resolveParentLinks [] = Just []
        resolveParentLinks (current : parent) = 
          if (current == "../")
            then (tailMay parent) >>= resolveParentLinks
            else do 
              resolvedParent <- (resolveParentLinks parent)
              return (current : resolvedParent)