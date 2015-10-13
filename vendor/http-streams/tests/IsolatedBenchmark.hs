--
-- Benchmark isolated code fragments
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

import qualified Blaze.ByteString.Builder as Builder (copyByteString,
                                                      copyByteString,
                                                      fromByteString,
                                                      fromByteString,
                                                      toByteString)
import qualified Blaze.ByteString.Builder.Char8 as Builder
import Control.Monad
import Criterion.Main
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import GHC.Conc
import Network.Http.Client
import Network.Http.ResponseParser (parseResponse)
import Network.Http.Types (Headers, Response (..), benchmark1, benchmark2)

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Debug.Trace
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

main :: IO ()
main = do
    GHC.Conc.setNumCapabilities 4

    x' <- S.readFile "tests/example2.txt"

    let Right p = parseOnly parseResponse x'
    let h = pHeaders p

    defaultMain
       [bench "original" (original h),
        bench "experiment" (experiment h)]

    putStrLn "Complete."


original :: Headers -> Pure
original h = do
    nf (S.length . benchmark1) h

experiment :: Headers -> Pure
experiment h = do
    nf (S.length . benchmark2) h

original' :: Headers -> IO ()
original' h = do
    replicateM_ 10000 (return $ S.length $ benchmark1 h)

experiment' :: Headers -> IO ()
experiment' h = do
    replicateM_ 10000 (return $ S.length $ benchmark2 h)

