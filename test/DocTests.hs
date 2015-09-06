module Main where

import Test.DocTest

-- Disabled for now because Stack support doesn't work properly (Ambiguous module name errors).
main :: IO ()
main = doctest ["-isrc"]
