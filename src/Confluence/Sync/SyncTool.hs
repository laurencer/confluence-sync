{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Confluence.Sync.SyncTool (
  sync
, ConfluenceConfig(..)
, confluenceXmlApi
) where
import Debug.Trace
import           Prelude hiding (readFile)

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Char
import           Data.String.Utils
import           Data.List (break, intercalate, nubBy, find, foldl', sortOn, (\\))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Tree.Zipper
import           Data.Tuple (swap)
import qualified Data.ByteString as BS
import           Data.MIME.Types

import           System.FilePath

import           Text.Heredoc

import           Confluence.Sync.LocalSite
import           Confluence.Sync.Content
import           Confluence.Sync.ReferenceResolver

import qualified Confluence.Sync.XmlRpc.Api as Api
import           Confluence.Sync.XmlRpc.Types
import           Confluence.Sync.XmlRpc.Requests (ApiCall, runApiCall)
import           Confluence.Sync.Internal.RateLimiter

data ConfluenceConfig = ConfluenceConfig {
  user          :: String
, password      :: String
, confluenceUrl :: String
  -- Used as a prefix for all pages.
, syncTitle     :: String
  -- Id of the page to use as the sync base.
, syncSpaceKey  :: String
, syncPageId    :: Maybe String
} deriving Show

-- Address of the XML-RPC Api root
confluenceXmlApi :: ConfluenceConfig -> String
confluenceXmlApi config = (confluenceUrl config) ++ "/rpc/xmlrpc"

createRootPage :: ConfluenceConfig -> ApiCall Page
createRootPage (ConfluenceConfig { syncSpaceKey, syncTitle }) = do
  let newRootPage = NewPage {
      newPageSpace        = syncSpaceKey
    , newPageParentId     = Nothing
    , newPageTitle        = syncTitle
    , newPageContent      = "Root page for synchronization (not initialized yet)"
    , newPagePermissions  = Nothing
  }
  liftIO . putStrLn $ "Creating new root page (\"" ++ syncTitle ++ "\") for synchronization (in space \"" ++ syncSpaceKey ++ "\")"
  newPage <- Api.createPage newRootPage
  liftIO . putStrLn $ "Root page (\"" ++ syncTitle ++ "\") successfully created (in space \"" ++ syncSpaceKey ++  "\"): page id = " ++ (show $ pageId newPage) ++ ", url = " ++ (show $ pageUrl newPage)
  return newPage

createOrFindPage :: ConfluenceConfig -> ApiCall Page
createOrFindPage (config@ConfluenceConfig { syncSpaceKey, syncTitle }) = do
  liftIO . putStrLn $ "No page id provided - looking up page \"" ++ syncTitle ++ "\" in \"" ++ syncSpaceKey ++ "\""
  findPage `catchError` (\_ -> createTheRootPage)
  where findPage = do
          page <- (Api.getPageByName syncSpaceKey syncTitle)
          liftIO . putStrLn $ "Found root page (by name lookup): page id = " ++ (show $ pageId page) ++ ", url = " ++ (show $ pageUrl page)
          return page
        createTheRootPage = do
          liftIO . putStrLn $ "Could not find page with name \"" ++ syncTitle ++ "\" (in space \"" ++ syncSpaceKey ++ "\"). Creating new page..."
          createRootPage config

-------------------------------------------------------------------------------
-- Meta Pages
-------------------------------------------------------------------------------

-- | True if the page is a meta page.
isMetaPage :: ConfluenceConfig -> String -> Bool
isMetaPage config other = other == (metaPageName config) || other == (metaTrashPageName config)

-- | Creates or updates all of the meta pages as necessary.
createMetaPages :: ConfluenceConfig -> Page -> ApiCall (Page, Page)
createMetaPages config rootPage = do
  metaPage  <- createOrFindMetaPage config rootPage
  trashPage <- createOrFindMetaTrashPage config metaPage
  return (metaPage, trashPage)

metaPageName :: ConfluenceConfig -> String
metaPageName (ConfluenceConfig { syncTitle }) = "Meta (" ++ syncTitle ++ ")"

createOrFindMetaPage :: ConfluenceConfig -> Page -> ApiCall Page
createOrFindMetaPage (config@ConfluenceConfig { syncSpaceKey }) (rootPage@Page { pageId = rootPageId, pageTitle = rootPageTitle }) = do
  liftIO . putStrLn $ "No page id provided - looking up page \"" ++ pageTitle ++ "\" in \"" ++ syncSpaceKey ++ "\""
  page <- findPage `catchError` (\_ -> createMetaPage)
  -- If the pages are not in the correct location - we require the user to manually move them.
  -- It means that someone has come in manually and moved them. The user should verify what has happened
  -- (we don't want to cause data loss or break something).
  if ((pageParentId page) /= rootPageId)
    then throwError $ "Metadata page (id = " ++ (pageId page) ++ ", name =\"" ++ pageTitle ++ "\") is not under the root page (id = " ++ rootPageId ++ ",name = \"" ++ rootPageTitle ++ "\"). Please move it to the correct location."
    else return ()
  return page
  where pageTitle = metaPageName config
        findPage = do
          page <- (Api.getPageByName syncSpaceKey pageTitle)
          liftIO . putStrLn $ "Found meta page (by name lookup): page id = " ++ (show $ pageId page) ++ ", url = " ++ (show $ pageUrl page)
          return page
        createMetaPage = do
          liftIO . putStrLn $ "Could not find page with name \"" ++ pageTitle ++ "\" (in space \"" ++ syncSpaceKey ++ "\"). Creating new page..."
          let newMetaPage = NewPage {
              newPageSpace        = syncSpaceKey
            , newPageParentId     = Just rootPageId
            , newPageTitle        = pageTitle
            , newPageContent      = "This page is the parent for all synchronization support pages (e.g. Trash)."
            , newPagePermissions  = Nothing
          }
          liftIO . putStrLn $ "Creating new meta page (\"" ++ pageTitle ++ "\") for synchronization (in space \"" ++ syncSpaceKey ++ "\")"
          newPage <- Api.createPage newMetaPage
          liftIO . putStrLn $ "Meta page (\"" ++ pageTitle ++ "\") successfully created (in space \"" ++ syncSpaceKey ++  "\"): page id = " ++ (show $ pageId newPage) ++ ", url = " ++ (show $ pageUrl newPage)
          return newPage

metaTrashPageName :: ConfluenceConfig -> String
metaTrashPageName (ConfluenceConfig { syncTitle }) = "Meta / Trash (" ++ syncTitle ++ ")"

createOrFindMetaTrashPage :: ConfluenceConfig -> Page -> ApiCall Page
createOrFindMetaTrashPage (config@ConfluenceConfig { syncSpaceKey }) (metaPage@Page { pageId = metaPageId, pageTitle = metaPageTitle }) = do
  liftIO . putStrLn $ "No page id provided - looking up page \"" ++ pageTitle ++ "\" in \"" ++ syncSpaceKey ++ "\""
  page <- findPage `catchError` (\_ -> createTrashPage)
  -- If the pages are not in the correct location - we require the user to manually move them.
  -- It means that someone has come in manually and moved them. The user should verify what has happened
  -- (we don't want to cause data loss or break something).
  if ((pageParentId page) /= metaPageId)
    then throwError $ "Trash page (id = " ++ (pageId page) ++ ", name =\"" ++ pageTitle ++ "\") is not under the meta page (id = " ++ metaPageId ++ ",name = \"" ++ metaPageTitle ++ "\"). Please move it to the correct location."
    else return ()
  return page
  where pageTitle = metaTrashPageName config
        findPage = do
          page <- (Api.getPageByName syncSpaceKey pageTitle)
          liftIO . putStrLn $ "Found trash page (by name lookup): page id = " ++ (show $ pageId page) ++ ", url = " ++ (show $ pageUrl page)
          return page
        createTrashPage = do
          liftIO . putStrLn $ "Could not find page with name \"" ++ pageTitle ++ "\" (in space \"" ++ syncSpaceKey ++ "\"). Creating new page..."
          let newMetaPage = NewPage {
              newPageSpace        = syncSpaceKey
            , newPageParentId     = Just metaPageId
            , newPageTitle        = pageTitle
            , newPageContent      = "All pages that have been deleted are moved to under this page (to hide them)."
            , newPagePermissions  = Nothing
          }
          liftIO . putStrLn $ "Creating new trash page (\"" ++ pageTitle ++ "\") for synchronization (in space \"" ++ syncSpaceKey ++ "\")"
          newPage <- Api.createPage newMetaPage
          liftIO . putStrLn $ "Trash page (\"" ++ pageTitle ++ "\") successfully created (in space \"" ++ syncSpaceKey ++  "\"): page id = " ++ (show $ pageId newPage) ++ ", url = " ++ (show $ pageUrl newPage)
          return newPage

-------------------------------------------------------------------------------
-- Content manipulation.
-------------------------------------------------------------------------------

-- | Creates placeholder pages for all new pages to be created.
--   These pages will be subsequently updated below.
createContentPage :: Page -> String -> ApiCall Page
createContentPage (Page { pageId = rootPageId, pageSpace }) title = do
  let newPage = NewPage {
      newPageSpace        = pageSpace
    , newPageParentId     = Just rootPageId
    , newPageTitle        = title
    , newPageContent      = newPagePlaceholderMessage
    , newPagePermissions  = Nothing
  }
  liftIO . putStrLn $ "Synchronizing - creating page \"" ++ title ++ "\""
  page <- Api.createPage newPage
  liftIO . putStrLn $ "Synchronizing - created page \"" ++ title ++ "\" (\"" ++ (pageUrl page) ++ "\")"
  return page

updateContentPage :: ConfluenceConfig -> (Map String String) -> ConfluenceZipper -> ApiCall Page
updateContentPage config renames zipper = do
  let pageSummary   = remotePage . label $ zipper
      pageId        = pageSummaryId pageSummary
      originalTitle = pageSummaryTitle pageSummary
      title         = Map.findWithDefault originalTitle originalTitle renames
      fullPath      = maybe "<root>" id $ (filePath . label) <$> (pageSource . pagePosition . label $ zipper)
      parentPageId  = maybe (pageSummaryParentId pageSummary) id ((pageSummaryId . remotePage . label) <$> (parent zipper))
  liftIO . putStrLn $ "Synchronizing - updating page \"" ++ title ++ "\" (\"" ++ fullPath ++ "\")"
  page              <- Api.getPage pageId
  localAttachments  <- liftIO $ getAttachments zipper
  remoteAttachments <- Api.getAttachments pageId
  attachmentMapping <- syncAttachments pageId localAttachments remoteAttachments
  localContents <- liftIO $ getPageContents zipper attachmentMapping
  let updatedPage = page { pageTitle = title, pageContent = localContents, pageParentId = parentPageId}
  if (page == updatedPage)
   then liftIO . putStrLn $ "Synchronizing - page \"" ++ title ++ "\" is identical to the local copy."
   else do
      liftIO . putStrLn $ "Synchronizing - page \"" ++ title ++ "\" is different and requires updating."
      Api.storePage updatedPage
      liftIO . putStrLn $ "Synchronizing - page \"" ++ title ++ "\" update successfully: " ++ (pageUrl page)
  return page

syncAttachments :: String -> [ LocalAttachment ] -> [ Attachment ] -> ApiCall [ (LocalAttachment, Attachment) ]
syncAttachments pageId localAttachments remoteAttachments = do
  let localRemoteNames = map attachmentRemoteName localAttachments
  let localRemotesNamesWithLocals = zip localRemoteNames localAttachments
  let nameToLocal = Map.fromList $ localRemotesNamesWithLocals
  let nameToRemote = Map.fromList $ zip (fmap attachmentFileName remoteAttachments) remoteAttachments

  let localSet = Set.fromList $ localRemoteNames
  let remoteSet = Set.fromList $ (fmap attachmentFileName remoteAttachments)
  let completeSet = localSet `Set.union` remoteSet

  let toRemove  = completeSet `Set.difference` localSet
  let toAdd     = completeSet `Set.difference` remoteSet
  let remaining = localSet `Set.intersection` remoteSet

  -- Log the attachment actions to take.
  liftIO $ putStrLn $ "Attachments to create: " ++ (if (Set.null toAdd) then "none." else "")
  liftIO . sequence $ map (\t -> putStrLn $ "  - " ++ show t) (Set.toList toAdd)
  liftIO $ putStrLn ""
  liftIO $ putStrLn $ "Attachments to delete: " ++ (if (Set.null toRemove) then "none." else "")
  liftIO . sequence $ map (\t -> putStrLn $ "  - " ++ show t) (Set.toList toRemove)
  liftIO $ putStrLn ""

  -- Actual perform the actions.
  forM (Set.toList toRemove) $ (\attachmentName -> do
    let attachment = nameToRemote Map.! attachmentName
    liftIO $ putStrLn $ "Removing attachment " ++ attachmentName
    (do
      Api.removeAttachment (attachmentPageId attachment) attachmentName
      liftIO $ putStrLn $ "Attachment removed: " ++ attachmentName
      ) `catchError` (\_ ->
        liftIO $ putStrLn $ "WARN: failed to remove attachment " ++ attachmentName ++ " from page (id = " ++ pageId ++ ")"
      )
    )

  added <- forM (Set.toList toAdd) $ (\attachmentName -> do
    let local = nameToLocal Map.! attachmentName
    liftIO $ putStrLn $ "Adding attachment " ++ attachmentName
    contents <- liftIO $ BS.readFile (filePath . label . resolvedAttachment $  local)
    let (mimeTypeGuess, encodingGuess) = guessType defaultmtd False attachmentName
    let contentType = maybe "application/unknown" id mimeTypeGuess
    remote <- Api.addAttachment pageId (NewAttachment attachmentName contentType) contents
    liftIO $ putStrLn $ "Attachment added: " ++ attachmentName
    return $ (local, remote)
    )

  -- Now we want to create a list of local files mapped to remote attachments.
  -- NB. the list of local files is more like a list of references (and thus there may be duplicates).
  let addedNameToRemote = Map.fromList $ (fmap (\a -> ((attachmentFileName a), a)) (fmap snd added))
  let allRemote = addedNameToRemote `Map.union` nameToRemote

  forM localRemotesNamesWithLocals $ (\(remoteName, local) -> do
      return $ (local, allRemote Map.! remoteName)
    )

trashContentPage :: Page -> PageSummary -> ApiCall Page
trashContentPage (trashPage@Page { pageId = trashPageId }) (PageSummary { pageSummaryId = pageId, pageSummaryTitle = title }) = do
  liftIO . putStrLn $ "Synchronizing - trashing page \"" ++ title ++ "\""
  page <- Api.getPage pageId
  Api.storePage page { pageContent = deletedPageMessage, pageParentId = trashPageId}
  liftIO . putStrLn $ "Synchronizing - page \"" ++ title ++ "\" has been moved to the trash successfully: " ++ (pageUrl page)
  return page

-------------------------------------------------------------------------------
-- Sync Messages
-------------------------------------------------------------------------------

newPagePlaceholderMessage = [here|
<ac:structured-macro ac:name="info">
  <ac:rich-text-body>
    <p>This page is currently being synchronized - please check back again soon.</p>
  </ac:rich-text-body>
</ac:structured-macro>
|]

deletedPageMessage = [here|
<ac:structured-macro ac:name="info">
  <ac:rich-text-body>
    <p>This page has been deleted and is no longer available.</p>
  </ac:rich-text-body>
</ac:structured-macro>
|]

-------------------------------------------------------------------------------
-- Sync implementation.
-------------------------------------------------------------------------------

sync :: Throttle -> ConfluenceConfig -> FilePath -> IO ()
sync throttle config path = do
  siteTree       <- buildSiteTree path
  let pageTree    = buildPageTree siteTree
  let pageZipper  = fromTree pageTree
  let localPages :: [PageZipper]
      localPages  = filter (notShadowReference . label) (traverseZipper pageZipper)
  -- Get all the potential page titles.
  let potentialPageTitles :: Set String
      potentialPageTitles = Set.fromList $ localPages >>= (potentialPageNames (syncTitle config))
  -- Get all the pages with their potential titles.
  let localPagesWithPotentialTitles :: [(PageZipper, [String])]
      localPagesWithPotentialTitles = zip localPages ((potentialPageNames (syncTitle config)) `fmap` localPages)
  -- Deduplicates potential page titles within the site.
  let flattenedPotentialPagesWithTitles :: [(PageZipper, String)]
      flattenedPotentialPagesWithTitles = localPagesWithPotentialTitles >>= (\(page, titles) -> (\title -> (page, title)) `fmap` titles)
  let localPagesWithUniqueTitles :: [(PageZipper, String)]
      localPagesWithUniqueTitles = nubBy (\a b -> (snd a) == (snd b)) flattenedPotentialPagesWithTitles
  -- Map from page to titles (sorted from most human friendly to most unique).
  let localPagesWithTitles :: Map PageZipper [String]
      localPagesWithTitles = (sortOn length) `Map.map` (foldl' (\m (page, title) -> Map.insertWithKey (\_ a b -> b ++ a) page [title] m) Map.empty localPagesWithUniqueTitles)
  -- Map from title to the corresponding page.
  let titleToLocalPage :: Map String PageZipper
      titleToLocalPage = Map.fromList (swap `fmap` localPagesWithUniqueTitles)

  putStrLn "Logging into Confluence"
  token <- Api.login (confluenceXmlApi config) (user config) (password config)
  putStrLn $ "Successfully logged into Confluence: token = " ++ token

  result <- runApiCall throttle (confluenceXmlApi config) token $ do
      rootPage <- case (syncPageId config) of
                    Just pageId -> Api.getPage pageId
                    Nothing     -> createOrFindPage config
      -- Create/find the meta pages.
      (metaPage, trashPage) <- createMetaPages config rootPage
      -- Now we have the root page get the descendants.
      descendants <- Api.getDescendents (pageId rootPage)
      let remotePages = filter (\p -> not $ (isMetaPage config) (pageSummaryTitle p)) descendants
      let remoteTitles = Set.fromList $ (syncTitle config) : (map pageSummaryTitle remotePages)

      -- Match each remote page with a potential local page.
      -- This allows us to do renames without creating/deleting excess pages.
      let matchedPages :: [(PageZipper, String)]
          matchedPages = catMaybes $ ((\t -> (\p -> (p, t)) `fmap` (Map.lookup t titleToLocalPage))) `fmap` (Set.toList remoteTitles)

      -- Calculate the possible renames.
      let possiblePageRenames :: [(String, [String])]
          possiblePageRenames = (\(_, others) -> not (null others)) `filter` ((\(p, t) -> (t, (fst $ (== t) `break` (localPagesWithTitles Map.! p)))) `fmap` matchedPages)

      -- Pages to rename.
      -- A page rename can be done where the page doesn't already exist.
      -- For each possible rename we need to lookup the page by name to see if it exists.
      renames <- catMaybes <$> (forM possiblePageRenames $ (\(originalName, others) -> do
          -- A name is optimal if it doesn't already exist.
          -- The list of other names is sorted by order of human-friendliness.
          optimalName <- findM (\other -> ((\_ -> Nothing) `fmap` (Api.getPageByName (syncSpaceKey config) other)) `catchError` (\_ -> return $ Just other)) others
          return $ (\optimal -> (originalName, optimal)) `fmap` optimalName
        ))
      let renameMap = Map.fromList renames

      -- Pages to create.
      -- Where we are creating a new page, we need to find the most optimal title for it.
      let pagesToCreate = (\\) (Map.keys localPagesWithTitles) (fst `fmap` matchedPages)
      titlesToCreate <- (Set.fromList . catMaybes) <$> (forM pagesToCreate $ (\page -> do
          let possibleTitles = localPagesWithTitles Map.! page
          -- A name is optimal if it doesn't already exist.
          -- The list of other names is sorted by order of human-friendliness.
          optimalName <- findM (\title -> ((\_ -> Nothing) `fmap` (Api.getPageByName (syncSpaceKey config) title)) `catchError` (\_ -> return $ Just title)) possibleTitles
          return optimalName
        ))

      let titlesAlreadyDeleted = Set.fromList $ map pageSummaryTitle (filter (\p -> (pageSummaryParentId p) == (pageId trashPage)) remotePages)

      -- Calculate the page-level actions to take.
      let titlesToUpdate = titlesToCreate `Set.union` (Set.fromList (snd `fmap` renames)) `Set.union` (Set.fromList (snd `fmap` matchedPages))

      -- The titles to remove are all remote pages which we are not going to update, and which are not being renamed, or already deleted.
      let titlesToRemove = (remoteTitles `Set.difference` titlesToUpdate) `Set.difference` (Set.fromList (fst `fmap` renames)) `Set.difference` titlesAlreadyDeleted

      -- Log the page-level actions that will be taken.
      liftIO $ putStrLn $ "Pages to create: " ++ (if (Set.null titlesToCreate) then "none." else "")
      liftIO . sequence $ map (\t -> putStrLn $ "  - " ++ show t) (Set.toList titlesToCreate)
      liftIO $ putStrLn ""
      liftIO $ putStrLn $ "Pages to rename: " ++ (if (null renames) then "none." else "")
      liftIO . sequence $ map (\t -> putStrLn $ "  - " ++ t) ((\(original, updated) -> original ++ " -> " ++ updated)`fmap` renames)
      liftIO $ putStrLn ""
      liftIO $ putStrLn $ "Pages to update: " ++ (if (Set.null titlesToUpdate) then "none." else "")
      liftIO . sequence $ map (\t -> putStrLn $ "  - " ++ show t) (Set.toList titlesToUpdate)
      liftIO $ putStrLn ""
      liftIO $ putStrLn $ "Pages to delete: " ++ (if (Set.null titlesToRemove) then "none." else "")
      liftIO . sequence $ map (\t -> putStrLn $ "  - " ++ show t) (Set.toList titlesToRemove)
      liftIO $ putStrLn ""

      -- Create all the placeholder pages first.
      createdPages <- sequence $ map (\title -> createContentPage rootPage title) (Set.toList titlesToCreate)
      let createdPageSummaries = pageSummaryFromPage <$> createdPages

      -- Create a list of page name to actual created page mappings.
      -- This is needed to map relative URLs/links between pages to absolute URLs.
      --
      -- This is the complete list of all possible pages (this is used for updating/creating links between pages).
      -- The root page is added in here because it will have the same name as the root directory page.
      let allRemotePages = remotePages ++ createdPageSummaries ++ [ (pageSummaryFromPage rootPage) ]
      let allRemotePagesMap :: Map String PageSummary
          allRemotePagesMap = Map.fromList $ zip (map pageSummaryTitle allRemotePages) allRemotePages
      let localToRemoteMap :: Map PageZipper PageSummary
          localToRemoteMap = (\title -> titleToLocalPage Map.! title) `Map.mapKeys` allRemotePagesMap
      let localToRemoteLookup :: PageZipper -> PageSummary
          localToRemoteLookup local = localToRemoteMap Map.! local

      -- Create the tree of pages to sync (and the zipper on that tree).
      let syncTree = pageTreeToSyncTree pageZipper localToRemoteLookup
      let syncZipper = fromTree syncTree

      -- Update the content of all pages (including renaming where appropriate).
      sequence $ map (updateContentPage config renameMap) (filter (notShadowReference . pagePosition . label) (traverseZipper syncZipper))

      -- Delete any old pages.
      sequence $ map (\title -> trashContentPage trashPage (allRemotePagesMap Map.! title)) (Set.toList titlesToRemove)

      ---
      liftIO $ putStrLn "Logging out of Confluence."
      Api.logout
      liftIO $ putStrLn "Logged out successfully."
  either (\err -> fail $ "ERROR: " ++ err) (\_ -> return ()) result

findM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM _ [] = return Nothing
findM f (x:xs) =
  (f x) >>= recurse
  where recurse (Just value) = return $ Just value
        recurse Nothing = findM f xs
