--
-- Benchmark code: fake connection using http-streams only
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# OPTIONS -fno-warn-unused-imports #-}

import Criterion.Main
import GHC.Conc
import Network.Http.Client

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.UTF8 as S
import Debug.Trace
import System.IO.Streams (OutputStream, stdout)
import qualified System.IO.Streams as Streams

import qualified BaselinePoint as Baseline (series)
import qualified CurrentPoint as Current (series)

main :: IO ()
main = do
    GHC.Conc.setNumCapabilities 4

    x' <- S.readFile "tests/example2.txt"

    defaultMain
       [bench "baseline" (Baseline.series x'),
        bench "experiment" (Current.series x')]

    putStrLn "Complete."

