--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Maintainer: Andrew Cowie
-- Stability: Experimental
--
-- /Overview/
--
-- A simple HTTP client library, using the Snap Framework's @io-streams@
-- library to handle the streaming I\/O. The @http-streams@ API is designed
-- for ease of use when querying web services and dealing with the result.
--
-- Given:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import System.IO.Streams (InputStream, OutputStream, stdout)
-- > import qualified System.IO.Streams as Streams
-- > import qualified Data.ByteString as S
--
-- and this library:
--
-- > import Network.Http.Client
--
-- the underlying API is straight-forward. In particular, constructing the
-- 'Request' to send is quick and to the point:
--
-- @
-- main :: IO ()
-- main = do
-- \    c <- 'openConnection' \"www.example.com\" 80
--
-- \    let q = 'buildRequest1' $ do
--                 'http' GET \"\/\"
--                 'setAccept' \"text/html\"
--
-- \    'sendRequest' c q 'emptyBody'
--
-- \    `receiveResponse` c (\\p i -> do
--         xm <- Streams.read i
--         case xm of
--             Just x    -> S.putStr x
--             Nothing   -> \"\")
--
-- \    'closeConnection' c
-- @
--
-- which would print the first chunk of the response back from the
-- server. Obviously in real usage you'll do something more interesting
-- with the 'Response' in the handler function, and consume the entire
-- response body from the InputStream ByteString.
--
-- Because this is all happening in 'IO' (the defining feature of
-- @io-streams@!), you can ensure resource cleanup on normal or
-- abnormal termination by using @Control.Exception@'s standard
-- 'Control.Exception.bracket' function; see 'closeConnection' for an
-- example. For the common case we have a utility function which
-- wraps @bracket@ for you:
--
-- @
-- foo :: IO ByteString
-- foo = 'withConnection' ('openConnection' \"www.example.com\" 80) doStuff
--
-- doStuff :: Connection -> IO ByteString
-- @
--
-- There are also a set of convenience APIs that do just that, along with
-- the tedious bits like parsing URLs. For example, to do an HTTP GET and
-- stream the response body to stdout, you can simply do:
--
-- @
--     'get' \"http:\/\/www.example.com\/file.txt\" (\\p i -> Streams.connect i stdout)
-- @
--
-- which on the one hand is \"easy\" while on the other exposes the the
-- 'Response' and InputStream for you to read from. Of course, messing
-- around with URLs is all a bit inefficient, so if you already have e.g.
-- hostname and path, or if you need more control over the request being
-- created, then the underlying @http-streams@ API is simple enough to use
-- directly.
--
module Network.Http.Client (
    -- * Connecting to server
    Hostname,
    Port,
    Connection,
    openConnection,
    openConnectionUnix,

    -- * Building Requests
    -- | You setup a request using the RequestBuilder monad, and
    -- get the resultant Request object by running 'buildRequest1'. The
    -- first call doesn't have to be to 'http', but it looks better when
    -- it is, don't you think?
    Method(..),
    RequestBuilder,
    buildRequest1,
    buildRequest,
    http,
    setHostname,
    setAccept,
    setAccept',
    setAuthorizationBasic,
    ContentType,
    setContentType,
    setContentLength,
    setExpectContinue,
    setTransferEncoding,
    setHeader,

    -- * Sending HTTP request
    Request,
    Response,
    getHostname,
    sendRequest,
    emptyBody,
    fileBody,
    inputStreamBody,
    encodedFormBody,

    -- * Processing HTTP response
    receiveResponse,
    receiveResponseRaw,
    UnexpectedCompression,
    StatusCode,
    getStatusCode,
    getStatusMessage,
    getHeader,
    debugHandler,
    concatHandler,
    concatHandler',
    HttpClientError(..),
    jsonHandler,

    -- * Resource cleanup
    closeConnection,
    withConnection,

    -- * Convenience APIs
    -- | Some simple functions for making requests with useful defaults.
    -- There's no @head@ function for the usual reason of needing to
    -- avoid collision with @Prelude@.
    --
    -- These convenience functions work with @http@ and @https@, but
    --  note that if you retrieve an @https@ URL, you /must/ wrap your
    -- @main@ function with 'OpenSSL.withOpenSSL' to initialize the
    -- native openssl library code.
    --
    URL,
    get,
    TooManyRedirects,
    post,
    postForm,
    put,

    -- * Secure connections
    openConnectionSSL,
    baselineContextSSL,
    modifyContextSSL,
    establishConnection,

    -- * Testing support
    makeConnection,
    Headers,
    getHeaders,
    getHeadersFull,

    -- * Deprecated
    getRequestHeaders
) where

import Network.Http.Types

import Network.Http.Connection
import Network.Http.Inconvenience
