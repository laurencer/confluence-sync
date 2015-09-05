module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Confluence/Sync/LocalFiles.hs"]
