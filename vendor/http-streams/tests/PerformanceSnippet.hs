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
{-# OPTIONS -fno-warn-unused-do-bind #-}

module Snippet where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)
import Network.Http.Client
import Network.URI (parseURI)

import Network.Http.Client

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Debug.Trace
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

import Network.Http.Connection (makeConnection)


main :: IO ()
main = do
    as <- getArgs
    let a = head as
    let n = read a :: Int

    x' <- S.readFile "tests/example2.txt"

    forM_ (replicate n True) (\_ -> basic x')
    putStr "\n"


basic :: ByteString -> IO ()
basic b' = do
    c <- fakeConnection b'

    let q = buildRequest1 $ do
                http GET "/"
                setAccept "text/plain"

    sendRequest c q emptyBody

    x' <- receiveResponse c concatHandler'
    if S.length x' > 0
        then putStr "."
        else putStr " "

    closeConnection c

fakeConnection :: ByteString -> IO Connection
fakeConnection b' = do
    o <- Streams.nullOutput
    i <- Streams.fromByteString b'

    makeConnection "www.example.com:80" (return ()) o i
