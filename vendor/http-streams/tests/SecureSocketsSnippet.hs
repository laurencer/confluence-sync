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
import OpenSSL (withOpenSSL)
import System.Exit (exitSuccess)
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams


main :: IO ()
main = example3

example1 = withOpenSSL $ do
    ctx <- baselineContextSSL
    c <- openConnectionSSL ctx "api.github.com" 443

    let q = buildRequest1 $ do
                http GET "/users/afcowie/orgs"
                setAccept "application/json"
    putStr $ show q

    sendRequest c q emptyBody

    receiveResponse c debugHandler

    closeConnection c

example2 = do
    get "https://api.github.com/users/afcowie" debugHandler
    get "https://github.com/afcowie" debugHandler

--
-- Does nesting 'withOpenSSL' break things? Apparently not:
--

example3 = withOpenSSL $ do
    get "https://api.github.com/users/afcowie" debugHandler
    get "https://github.com/afcowie" debugHandler

