import           Data.Maybe

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath

import           Confluence.Sync.SyncTool
import           Confluence.Sync.RateLimiter
import           Confluence.Sync.XmlRpc.Api as Api
import           Confluence.Sync.XmlRpc.Requests
import           Confluence.Sync.XmlRpc.Types

testSyncRoot = "Test Title"

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
  result <- runApiCall throttle (confluenceXmlApi config) token $ do
    Api.getPageByName spaceKey "Meta (Test Title)"
    Api.getPageByName spaceKey "Meta / Trash (Test Title)"
    Api.getPageByName spaceKey "About / Index - Test Title"
    Api.getPageByName spaceKey "Category / Index - Test Title"
    Api.logout
  either (\err -> fail $ "ERROR: " ++ err) (\_ -> return ()) result
