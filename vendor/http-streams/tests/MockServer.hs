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
{-# LANGUAGE PackageImports    #-}
{-# OPTIONS -fno-warn-dodgy-imports #-}

module MockServer (runMockServer, localPort) where

{-
    Per http://hackage.haskell.org/trac/ghc/ticket/7167, we suppress
    the warning resulting from this line, necessary on <7.6
-}
import Prelude hiding (catch)

import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException)
import Control.Monad.CatchIO (catch)
import "mtl" Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)
import Filesystem (getSize)
import Filesystem.Path.CurrentOS (decodeString)
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.IO (hFlush, hPutStrLn, stderr)

import Network.Http.Client (Hostname, Port)

localHost = "localhost" :: Hostname
localPort = 56981 :: Port

main :: IO ()
main = go

{-
    Binding the port to the IPv4 localhost appears to settle the problem
    of localhost resolving ambigiously. If that doesn't work, we can
    comment out the setBind and the resultant 0.0.0.0 does seem to work.
-}
go :: IO ()
go = httpServe c site
  where
    c = setAccessLog ConfigNoLog $
        setErrorLog ConfigNoLog $
        setHostname localHost $
        setBind localHost $
        setPort (fromIntegral localPort) $
        setVerbose False emptyConfig


runMockServer :: IO ()
runMockServer = do
    _ <- forkIO go
    threadDelay 2000000
    return ()

--
-- Top level URL routing logic.
--

site :: Snap ()
site = catch
    (routeRequests)
    (\e -> serveError "Splat\n" e)

routeRequests :: Snap ()
routeRequests =
    route
            [("resource/:id", serveResource),
             ("static/:id", method GET serveStatic),
             ("time", serveTime),
             ("", ifTop handleAsText),
             ("bounce", serveRedirect),
             ("local", serveLocalRedirect),
             ("loop", serveRedirectEndlessly),
             ("empty", serveWithoutContent),
             ("postbox", method POST handlePostMethod),
             ("size", handleSizeRequest),
             ("api", handleRestfulRequest),
             ("cookies", serveRepeatedResponseHeaders)]
    <|> serveNotFound


serveResource :: Snap ()
serveResource = do
    r <- getRequest

    let m = rqMethod r
    case m of
        GET     -> handleGetMethod
        PUT     -> handlePutWithExpectation
        _       -> serveMethodNotAllowed


serveStatic :: Snap ()
serveStatic = do
    im' <- getParam "id"

    let i' = fromMaybe "" im'
    let f' = S.concat ["tests/", i']
    let f = S.unpack f'

    l <- liftIO $ getSize $ decodeString f

    let t = fileType defaultMimeTypes f
    modifyResponse $ setContentType t
    modifyResponse $ setContentLength $ fromIntegral l
    b' <- liftIO $ S.readFile f
    writeBS b'


serveTime :: Snap ()
serveTime = do
    writeBS "Sun 30 Dec 12, 05:39:56.746Z\n"


--
-- Dispatch normal GET requests based on MIME type.
--

handleGetMethod :: Snap ()
handleGetMethod = do
    r <- getRequest
    let mime0 = getHeader "Accept" r

    case mime0 of
        Just "text/html"        -> handleAsBrowser
        _                       -> handleAsText


handleAsBrowser :: Snap ()
handleAsBrowser = do
    modifyResponse $ setResponseStatus 200 "OK"
    modifyResponse $ setContentType "text/html; charset=UTF-8"
    modifyResponse $ setHeader "Cache-Control" "max-age=1"
    sendFile "tests/hello.html"


handleAsText :: Snap ()
handleAsText = do
    modifyResponse $ setContentType "text/plain"
    writeBS "Sounds good to me\n"


handleRestfulRequest :: Snap ()
handleRestfulRequest = do
    modifyResponse $ setResponseStatus 200 "OK"
    modifyResponse $ setContentType "application/json"

    sendFile "tests/data-eu-gdp.json"


serveRedirect :: Snap ()
serveRedirect = do
    modifyResponse $ setResponseStatus 307 "Temporary Redirect"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setHeader "Location" r'
  where
    r' = S.concat ["http://", localHost, ":", S.pack $ show $ localPort, "/time"]

serveLocalRedirect :: Snap ()
serveLocalRedirect = do
    modifyResponse $ setResponseStatus 307 "Temporary Redirect"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setHeader "Location" r'
  where
    r' = S.pack "/time"

serveRedirectEndlessly :: Snap ()
serveRedirectEndlessly = do
    modifyResponse $ setResponseStatus 307 "Temporary Redirect"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setHeader "Location" r'
  where
    r' = S.concat ["http://", localHost, ":", S.pack $ show $ localPort, "/loop"]

{-
    Attempt to test the bug with 204 No Content not closing in absence of a
    Content-Length header, however Snap automatically adds one, it seems. So,
    after the fact, this is unused and the case is tested in
    TestServer.testDevoidOfContent.
-}

serveWithoutContent :: Snap ()
serveWithoutContent = do
    modifyResponse $ setResponseStatus 204 "No Content"
    modifyResponse $ setHeader "Cache-Control" "no-cache"


serveRepeatedResponseHeaders :: Snap ()
serveRepeatedResponseHeaders = do
    modifyResponse $ addHeader "Set-Cookie" "stone=diamond"
    modifyResponse $ addHeader "Set-Cookie" "metal=tungsten"

handlePostMethod :: Snap ()
handlePostMethod = do
    setTimeout 5
    modifyResponse $ setResponseStatus 201 "Created"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setHeader "Location" "http://server.example.com/something/788"
    modifyResponse $ setContentType "text/plain"

    b' <- readRequestBody 1024
    writeLBS b'


handlePutWithExpectation :: Snap ()
handlePutWithExpectation = do
    setTimeout 5
    modifyResponse $ setResponseStatus 201 "Created"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setContentType "text/plain"

    b' <- readRequestBody 1024
    writeLBS b'


handleSizeRequest :: Snap ()
handleSizeRequest = do
    r <- getRequest
    let mm = getHeader "Content-Type" r

    t <- case mm of
        Just m -> return m
        _      -> do
            serveUnsupported
            return ""

    modifyResponse $ setResponseStatus 200 "OK"
    modifyResponse $ setContentType t

    b' <- readRequestBody 65536
    writeBS $ S.pack $ show $ L.length b'


updateResource :: Snap ()
updateResource = do
    bs' <- readRequestBody 4096
    let b' = fromLazy bs'

    im' <- getParam "id"
    let i' = fromMaybe "0" im'

    -- TODO something

    modifyResponse $ setResponseStatus 204 "Updated" -- "No Content"
    modifyResponse $ setHeader "Cache-Control" "no-cache"
    modifyResponse $ setContentLength 0
    return ()
  where
    fromLazy ls' = S.concat $ L.toChunks ls'


serveNotFound :: Snap a
serveNotFound = do
    modifyResponse $ setResponseStatus 404 "Not Found"
    modifyResponse $ setHeader "Content-Type" "text/html"

    writeBS "404 Not Found"

    r <- getResponse
    finishWith r


serveBadRequest :: Snap ()
serveBadRequest = do
    modifyResponse $ setResponseStatus 400 "Bad Request"
    writeBS "400 Bad Request\n"


serveMethodNotAllowed :: Snap ()
serveMethodNotAllowed = do
    modifyResponse $ setResponseStatus 405 "Method Not Allowed"
    modifyResponse $ setHeader "Allow" "GET, POST, PUT"

    writeBS "405 Method Not Allowed\n"
    r <- getResponse
    finishWith r


serveUnsupported :: Snap ()
serveUnsupported = do
    modifyResponse $ setResponseStatus 415 "Unsupported Media Type"
    writeBS "415 Unsupported Media Type\n"
    r <- getResponse
    finishWith r


--
-- The exception will be dumped to the server's stdout, while the supplied
-- message will be sent out with the response (ideally only for debugging
-- purposes, but easier than looking in log/error.log for details).
--

serveError :: ByteString -> SomeException -> Snap ()
serveError x' e = do
    debug msg
    modifyResponse $ setResponseStatus 500 "Internal Server Error"
    writeBS x'
    r <- getResponse
    finishWith r
  where
    msg = show (e :: SomeException)


debug :: String -> Snap ()
debug cs = do
    liftIO $ do
        hPutStrLn stderr ""
        hPutStrLn stderr cs
        hFlush stderr


