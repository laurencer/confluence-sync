module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc"
  , "src/Confluence/Sync/ReferenceResolver.hs"
  -- Disabled for now because Stack support doesn't work properly
  -- (Ambiguous module name errors with Crypto.Hash).
  -- , "src/Confluence/Sync/Content.hs"
  ]
