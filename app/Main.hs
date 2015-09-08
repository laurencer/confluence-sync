{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Options.Applicative

import           Text.Heredoc (here)

import           System.Environment

import           Confluence.Sync.SyncTool
import           Confluence.Sync.Internal.RateLimiter

data CommandLineArguments = CommandLineArguments {
  argsSpaceKey        :: String
, argsPageTitle       :: String
, argsPageId          :: Maybe String
, argsActionMinDelay  :: Int
, argsActionMaxDelay  :: Int
, argsRequestBackoff  :: Rational
, argsActionLimit     :: Int
, argsSyncDirectory   :: String
}

commandLineArguments :: Parser CommandLineArguments
commandLineArguments = CommandLineArguments 
   <$> strOption 
          (long "space-id"
        <> metavar "<SPACE ID>" 
        <> help "The id of the Confluence Space to synchronise with (typically in the URL `/display/<space id>/<page name>`)." )
   <*> strOption
          (long "page-name" 
        <> metavar "<PAGE TITLE>" 
        <> help "The name of the page that all content should be synced under." )
   <*> (optional $ strOption 
          (long "page-id" 
        <> metavar "<PAGE ID>" 
        <> help "Optional page id of the sync page (the id is usually found when editing the page in the URL `/pages/editpage.action?pageId=<page id>`)" ))
   <*> ((option auto)
          (long "min-delay-between-requests" 
        <> metavar "<DELAY IN MILLIS>" 
        <> help "Minimum time to wait between each API request in milliseconds."
        <> (value 2000)
        <> hidden
        <> (showDefaultWith (\ms -> (show ms) ++ " ms"))))
   <*> ((option auto)
          (long "max-delay-between-requests" 
        <> metavar "<DELAY IN MILLIS>" 
        <> help "Maximum time to wait between each API request in milliseconds."
        <> (value 60000)
        <> hidden
        <> (showDefaultWith (\ms -> (show ms) ++ " ms"))))
   <*> ((option auto)
          (long "request-backoff" 
        <> metavar "<MULTIPLIER>" 
        <> help "Multiplier for the API request time to calculate the delay before starting next action."
        <> (value (3 / 2))
        <> hidden
        <> (showDefaultWith (\ms -> "(" ++ (show ms) ++ ") x"))))
   <*> ((option auto)
          (long "max-number-of-requests" 
        <> metavar "<MAX # of REQUESTS>" 
        <> help "Maximum number of API requests that are allowed to be made."
        <> (value 10000)
        <> hidden
        <> (showDefaultWith (\c -> (show c) ++ " reqs"))))
   <*> argument str 
          (metavar "<SYNC DIRECTORY>" 
        <> help "Directory containing the website/content to sync to Confluence")

main :: IO ()
main = do
  let opts = info (helper <*> commandLineArguments) (fullDesc 
                  <> progDesc shortProgramDescription
                  <> footer fullProgramDescription
                  <> header "confluence-sync-tool - syncs HTML/Markdown content to Confluence")
  args              <- execParser opts
  confluenceUrl     <- getEnv "CONFLUENCE_URL"
  username          <- getEnv "CONFLUENCE_USER"
  password          <- getEnv "CONFLUENCE_PASSWORD"
  throttle          <- newThrottle (argsActionLimit args) (argsActionMinDelay args) (argsActionMaxDelay args) (argsRequestBackoff args)
  putStrLn $ "Using Confluence URL: " ++ confluenceUrl
  putStrLn $ "Using user: " ++ username
  let config = ConfluenceConfig username password confluenceUrl (argsPageTitle args) (argsSpaceKey args) (argsPageId args)
  sync throttle config (argsSyncDirectory args)
  return ()

shortProgramDescription = [here|
Synchronises all of the pages in the sync-directory to the given space and places them all under the specified page.
|]

fullProgramDescription = [here|
Expects the following environment variables to be set:


CONFLUENCE_URL="confluence server address"  (e.g. `http://192.168.99.100:8090/`)
CONFLUENCE_USER="confluence username"       (e.g. `admin`)
CONFLUENCE_PASSWORD="confluence password"   (e.g. `password123`)

|]