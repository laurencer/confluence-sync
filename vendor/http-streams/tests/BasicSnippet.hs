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

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

-- Obviously don't need all those imports, but useful for experimenting

import Network.Http.Client

main :: IO ()
main = do
    c <- openConnection "kernel.operationaldynamics.com" 58080

    let q = buildRequest1 $ do
                http GET "/time"
                setAccept "text/plain"

    sendRequest c q emptyBody

    receiveResponse c debugHandler

    closeConnection c
