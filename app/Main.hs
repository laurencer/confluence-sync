module Main where

import System.Environment

import Confluence.Sync.SyncTool

main :: IO ()
main = do
  args              <- getArgs
  workingDirectory  <- if length args == 1 
                        then return (head args) 
                        else error "Usage: confluence-sync-exe <path to sync>"
  confluenceUrl     <- getEnv "CONFLUENCE_URL"
  username          <- getEnv "CONFLUENCE_USER"
  password          <- getEnv "CONFLUENCE_PASSWORD"
  putStrLn $ "Using Confluence URL: " ++ confluenceUrl
  putStrLn $ "Using user: " ++ username
  let config = ConfluenceConfig username password confluenceUrl "Test Sync Page" "ds" Nothing
  sync config workingDirectory
  return ()
