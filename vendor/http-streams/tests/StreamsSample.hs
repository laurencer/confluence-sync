--
-- Benchmark code: sample request using http-streams
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module StreamsSample (sampleViaHttpStreams) where

import Data.Maybe (fromMaybe)
import Network.Http.Client

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

main :: IO ()
main = do
    sampleViaHttpStreams

sampleViaHttpStreams :: IO ()
sampleViaHttpStreams = do
    c <- openConnection "localhost" 80

    let q = buildRequest1 $ do
                http GET "/"
                setAccept "text/html"

    sendRequest c q emptyBody

    receiveResponse c (\p i ->
        Streams.withFileAsOutput
            "/tmp/http-streams.out"
            (\o -> do
                Streams.write (Just (S.pack $ show p)) o
                Streams.connect i o))

    closeConnection c

