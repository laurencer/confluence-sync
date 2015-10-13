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
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module Snippet where

import Control.Exception (bracket)
import Data.Maybe (fromMaybe)

import Network.Http.Client

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Debug.Trace
import System.Exit (exitSuccess)
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams


main :: IO ()
main = do

    putStrLn "---- Basic API ----"
    basic

    putStrLn "---- Resource cleanup ----"
    b' <- resource
    S.putStrLn b'

    putStrLn "---- Convenience API ----"
    express

    putStrLn "\n---- Done ----"

{-
    Explore with a simple HTTP request against localhost (where we
    already have an Apache server running; that will need to be more
    sophisticated once we start writing real tests.
-}

basic :: IO ()
basic = do
    c <- openConnection "kernel.operationaldynamics.com" 58080
    putStrLn $ show c

    let q = buildRequest1 $ do
                http GET "/time"
                setAccept "text/plain"
    putStr $ show q
            -- Requests [headers] are terminated by a double newline
            -- already. We need a better way of emitting debug
            -- information mid-stream from this library.

    sendRequest c q emptyBody

    receiveResponse c (\p i -> do
        putStr $ show p

        x <- Streams.read i
        putStr $ S.unpack $ fromMaybe "" x)

    closeConnection c

{-
    One of the deisgn features of io-streams is their use of the
    standard IO monad exception handling facilities. This example
    doesn't do much yet, but shows the basic usage pattern. Presumably
    the resulant ByteString (in this case) bubbling out of doStuff would
    be returned to the calling program to then be put to some use.
-}

resource :: IO ByteString
resource = bracket
    (openConnection "www.httpbin.org" 80)
    (closeConnection)
    (doStuff)


-- Now actually use the supplied Connection object to further
-- exercise the API. We'll do a PUT this time.

doStuff :: Connection -> IO ByteString
doStuff c = do
    let q = buildRequest1 $ do
                http PUT "/put"
                setAccept "*/*"
                setContentType "text/plain"
                setContentLength 12

    sendRequest c q (\o ->
        Streams.write (Just "Hello World\n") o)

    y' <- receiveResponse c (\_ i -> do
        x' <- Streams.read i
        return $ fromMaybe "" x')

    return y'


{-
    Experiment with a convenience API. This is very much in flux,
    with the open question being what type to return, if any.
-}

express :: IO ()
express = do
    get "http://kernel.operationaldynamics.com/yaminabe/" (\p i -> do
        putStr $ show p
        Streams.connect i stdout)

    put "http://www.httpbin.org/put" "text/plain" (fileBody "tests/hello.txt") (\p i -> do
        putStr $ show p
        Streams.connect i stdout)

    postForm "http://www.httpbin.org/post" [("name","Kermit"),("role","Stage & Screen")] (\p i -> do
        putStr $ show p
        Streams.connect i Streams.stdout)

