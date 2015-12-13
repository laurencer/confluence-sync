{-# LANGUAGE QuasiQuotes #-}

import           Control.Exception (evaluate)

import           Data.Maybe
import           Data.List

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Random

import           Test.Hspec

import           Text.InterpolatedString.Perl6 (qq)
import           Text.HandsomeSoup
import           Text.XML.HXT.Core

import           Confluence.Sync.SyncTool
import           Confluence.Sync.Internal.RateLimiter
import           Confluence.Sync.XmlRpc.Api as Api
import           Confluence.Sync.XmlRpc.Requests
import           Confluence.Sync.XmlRpc.Types

-- | Extracts an environment variable or exits to skip tests if not there.
env :: String -> IO String
env name = do
  value <- lookupEnv name
  case value of
    Just envValue -> return envValue
    Nothing       -> do
      putStrLn $ name ++ " env variable not found. Skipping integration tests."
      exitFailure

main = do
  putStrLn "Running tests..."
  randomGen    <- newStdGen
  let testSyncRoot = take 10 $ randomRs ('a','z') randomGen
  putStrLn $ "Using sync root: " ++ testSyncRoot
  confluenceUrl <- env "CONFLUENCE_TEST_URL"
  username      <- env "CONFLUENCE_TEST_USER"
  password      <- env "CONFLUENCE_TEST_PASSWORD"
  spaceKey      <- env "CONFLUENCE_TEST_SPACE"
  return $ (confluenceUrl, username, password, spaceKey)
  throttle      <- newThrottle 1000 0 1000 1
  let config = ConfluenceConfig username password confluenceUrl testSyncRoot spaceKey Nothing
  currentDirectory <- getCurrentDirectory
  sync throttle config (currentDirectory </> "test" </> "sample")
  token <- Api.login (confluenceXmlApi config) username password
  let invokeApi call = do
        result <- runApiCall throttle (confluenceXmlApi config) token call
        either (\err -> fail $ "ERROR: " ++ err) (return) result
  tests invokeApi spaceKey testSyncRoot
  tests invokeApi spaceKey testSyncRoot
  invokeApi Api.logout

tests:: (ApiCall Page -> IO Page)
                      -> String
                      -> String
                      -> IO ()
tests call spaceKey syncRoot = hspec $ do
  describe "Sample Site" $ do
    it "should have a custom root page" $ do
      call $ Api.getPageByName spaceKey [qq|{syncRoot}|]
      return ()
    it "should have a meta page" $ do
      call $ Api.getPageByName spaceKey [qq|Meta ({syncRoot})|]
      return ()
    it "should have a trash page" $ do
      call $ Api.getPageByName spaceKey [qq|Meta / Trash ({syncRoot})|]
      return ()
    it "should have a page at: About" $ do
      page <- call $ Api.getPageByName spaceKey [qq|About|]
      let content = pageContent page
      let uniqueWord = "UNIQUEWORD_ABOUT_DO_NOT_REMOVE"
      let correctPage = isInfixOf uniqueWord content
      if correctPage then return () else fail ("ERROR: could not find " ++ uniqueWord ++ " in\n" ++ content)
      return ()
    it "should have a page at: About / Nested / About" $ do
      page <- call $ Api.getPageByName spaceKey [qq|About (Nested)|]
      let content = pageContent page
      let uniqueWord = "UNIQUEWORD_ABOUT_NESTED_DO_NOT_REMOVE"
      let correctPage = isInfixOf uniqueWord content
      if correctPage then return () else fail ("ERROR: could not find " ++ uniqueWord ++ " in\n" ++ content)
      return ()
    it "should have a page at: Category / About" $ do
      page <- call $ Api.getPageByName spaceKey [qq|About (Category)|]
      let content = pageContent page
      let uniqueWord = "UNIQUEWORD_ABOUT_CATEGORY_DO_NOT_REMOVE"
      let correctPage = isInfixOf uniqueWord content
      if correctPage then return () else fail ("ERROR: could not find " ++ uniqueWord ++ " in\n" ++ content)
      return ()
    it "should have a page at: Category / Nested / About" $ do
      page <- call $ Api.getPageByName spaceKey [qq|About (Category / Nested - {syncRoot})|]
      let content = pageContent page
      let uniqueWord = "UNIQUEWORD_ABOUT_CATEGORY_NESTED_DO_NOT_REMOVE"
      let correctPage = isInfixOf uniqueWord content
      if correctPage then return () else fail ("ERROR: could not find " ++ uniqueWord ++ " in\n" ++ content)
      return ()
    it "should have a page at: Category" $ do
      call $ Api.getPageByName spaceKey [qq|Category|]
      return ()
    it "should have a page at: Category / My Nested Page" $ do
      call $ Api.getPageByName spaceKey [qq|My Nested Page|]
      return ()
    it "should have a page at: Sample" $ do
      call $ Api.getPageByName spaceKey [qq|Sample|]
      return ()
