--
-- Benchmark using fake connection
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module BaselinePoint (series, actual) where

import Control.Monad
import "http-streams" Network.Http.Client
            -- as installed

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.UTF8 as S
import Debug.Trace
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

series :: ByteString -> IO ()
series x' = do
    replicateM_ 1000 (actual x')


actual :: ByteString -> IO ()
actual x' = do
    c <- fakeConnection x'

    let q = buildRequest1 $ do
                http GET "/bucket42/object149"
                setAccept "text/plain"

    sendRequest c q emptyBody

    receiveResponse c (\p i -> do
        n <- Streams.nullOutput
        Streams.write (Just $ S.pack $ show p) n
        Streams.connect i n)
    return ()


fakeConnection :: ByteString -> IO Connection
fakeConnection x' = do
    o <- Streams.nullOutput
    i <- Streams.fromByteString x'

    c <- makeConnection "swift.example.com" (return ()) o i
    return c

