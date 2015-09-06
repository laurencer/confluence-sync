module Main where

import Test.DocTest

-- Disabled for now because Stack support doesn't work properly (Ambiguous module name errors).
main :: IO ()
main = putStrLn "Skipping doctests..."  -- doctest ["-isrc", "src/Confluence/Sync/SyncTool.hs"]
