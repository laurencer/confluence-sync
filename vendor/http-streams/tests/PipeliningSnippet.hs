--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module Snippet where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Network.Http.Client

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Debug.Trace
import System.Exit (exitSuccess)
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams


main :: IO ()
main = do
    c <- openConnection "kernel.operationaldynamics.com" 58080

    let q1 = buildRequest1 $ do
                http GET "/time?id=1"
                setAccept "text/plain"

    let q2 = buildRequest1 $ do
                http GET "/time?id=2"
                setAccept "text/plain"

    let q3 = buildRequest1 $ do
                http GET "/time?id=3"
                setAccept "text/plain"

    sendRequest c q1 emptyBody
    threadDelay 1000000
    sendRequest c q2 emptyBody
    threadDelay 1000000
    sendRequest c q3 emptyBody

    receiveResponse c debugHandler
    receiveResponse c debugHandler
    receiveResponse c debugHandler

    closeConnection c

