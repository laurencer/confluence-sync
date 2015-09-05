{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Client
-- Copyright   :  (c) Bjorn Bringert 2003
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- This module contains the client functionality of XML-RPC.
-- The XML-RPC specifcation is available at <http://www.xmlrpc.com/spec>.
--
-- A simple client application:
--
-- > import Network.XmlRpc.Client
-- >
-- > server = "http://localhost/~bjorn/cgi-bin/simple_server"
-- >
-- > add :: String -> Int -> Int -> IO Int
-- > add url = remote url "examples.add"
-- >
-- > main = do
-- >        let x = 4
-- >            y = 7
-- >        z <- add server x y
-- >        putStrLn (show x ++ " + " ++ show y ++ " = " ++ show z)
--
-----------------------------------------------------------------------------

module Network.XmlRpc.Client
    (
     remote, remoteWithHeaders,
     call, callWithHeaders,
     Remote
    ) where

import           Network.XmlRpc.Internals

import           Data.Functor               ((<$>))
import           Data.Maybe
import           Data.Int
import           Network.URI
import           Text.Read.Compat           (readMaybe)

import           Network.Http.Client        (Method (..), Request,
                                             baselineContextSSL, buildRequest,
                                             closeConnection, getStatusCode,
                                             getStatusMessage, http, openConnection,
                                             inputStreamBody, openConnectionSSL,
                                             receiveResponse, sendRequest,
                                             setAuthorizationBasic,
                                             setContentType, setContentLength, setHeader)
import           OpenSSL
import qualified System.IO.Streams          as Streams

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString, fromChunks,
                                                    unpack, length)
import qualified Data.ByteString.Lazy.UTF8  as U

-- | Gets the return value from a method response.
--   Throws an exception if the response was a fault.
handleResponse :: Monad m => MethodResponse -> m Value
handleResponse (Return v) = return v
handleResponse (Fault code str) = fail ("Error " ++ show code ++ ": " ++ str)

type HeadersAList = [(BS.ByteString, BS.ByteString)]

-- | Sends a method call to a server and returns the response.
--   Throws an exception if the response was an error.
doCall :: String -> HeadersAList -> MethodCall -> Err IO MethodResponse
doCall url headers mc =
    do
    let req = renderCall mc
    resp <- ioErrorToErr $ post url headers req
    parseResponse (BSL.unpack resp)

-- | Low-level method calling function. Use this function if
--   you need to do custom conversions between XML-RPC types and
--   Haskell types.
--   Throws an exception if the response was a fault.
call :: String -- ^ URL for the XML-RPC server.
     -> String -- ^ Method name.
     -> [Value] -- ^ The arguments.
     -> Err IO Value -- ^ The result
call url method args = doCall url [] (MethodCall method args) >>= handleResponse

-- | Low-level method calling function. Use this function if
--   you need to do custom conversions between XML-RPC types and
--   Haskell types. Takes a list of extra headers to add to the
--   HTTP request.
--   Throws an exception if the response was a fault.
callWithHeaders :: String -- ^ URL for the XML-RPC server.
                -> String -- ^ Method name.
                -> HeadersAList -- ^ Extra headers to add to HTTP request.
                -> [Value] -- ^ The arguments.
                -> Err IO Value -- ^ The result
callWithHeaders url method headers args =
    doCall url headers (MethodCall method args) >>= handleResponse


-- | Call a remote method.
remote :: Remote a =>
          String -- ^ Server URL. May contain username and password on
                 --   the format username:password\@ before the hostname.
       -> String -- ^ Remote method name.
       -> a      -- ^ Any function
                 -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                 -- t1 -> ... -> tn -> IO r@
remote u m = remote_ (\e -> "Error calling " ++ m ++ ": " ++ e) (call u m)

-- | Call a remote method. Takes a list of extra headers to add to the HTTP
--   request.
remoteWithHeaders :: Remote a =>
                     String   -- ^ Server URL. May contain username and password on
                              --   the format username:password\@ before the hostname.
                  -> String   -- ^ Remote method name.
                  -> HeadersAList -- ^ Extra headers to add to HTTP request.
                  -> a        -- ^ Any function
                              -- @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
                              -- t1 -> ... -> tn -> IO r@
remoteWithHeaders u m headers =
    remote_ (\e -> "Error calling " ++ m ++ ": " ++ e)
            (callWithHeaders u m headers)

class Remote a where
    remote_ :: (String -> String)        -- ^ Will be applied to all error
                                         --   messages.
            -> ([Value] -> Err IO Value)
            -> a

instance XmlRpcType a => Remote (IO a) where
    remote_ h f = handleError (fail . h) $ f [] >>= fromValue

instance (XmlRpcType a, Remote b) => Remote (a -> b) where
    remote_ h f x = remote_ h (\xs -> f (toValue x:xs))



--
-- HTTP functions
--

userAgent :: BS.ByteString
userAgent = "Haskell XmlRpcClient/0.1"

-- | Post some content to a uri, return the content of the response
--   or an error.
-- FIXME: should we really use fail?
post :: String -> HeadersAList -> BSL.ByteString -> IO U.ByteString
post url headers content = do
    uri <- maybeFail ("Bad URI: '" ++ url ++ "'") (parseURI url)
    let a = uriAuthority uri
    auth <- maybeFail ("Bad URI authority: '" ++ show (fmap showAuth a) ++ "'") a
    post_ uri auth headers content
  where showAuth (URIAuth u r p) = "URIAuth "++u++" "++r++" "++p

-- | Post some content to a uri, return the content of the response
--   or an error.
-- FIXME: should we really use fail?
post_ :: URI -> URIAuth -> HeadersAList -> BSL.ByteString -> IO U.ByteString
post_ uri auth headers content = withOpenSSL $ do
    let hostname = BS.pack (uriRegName auth)
        port     = fromMaybe 443 (readMaybe $ drop 1 $ uriPort auth)

    c <- case init $ uriScheme uri of
        "http"  ->
            openConnection hostname port
        "https" -> do
            ctx <- baselineContextSSL
            openConnectionSSL ctx hostname port
        x -> fail ("Unknown scheme: '" ++ x ++ "'!")

    req  <- request uri auth headers (BSL.length content)
    body <- inputStreamBody <$> Streams.fromLazyByteString content

    _ <- sendRequest c req body

    s <- receiveResponse c $ \resp i -> do
        case getStatusCode resp of
          200 -> readLazyByteString i
          _   -> fail (show (getStatusCode resp) ++ " " ++ BS.unpack (getStatusMessage resp))

    closeConnection c

    return s

readLazyByteString :: Streams.InputStream BS.ByteString -> IO U.ByteString
readLazyByteString i = BSL.fromChunks <$> go
  where
    go :: IO [BS.ByteString]
    go = do
      res <- Streams.read i
      case res of
        Nothing -> return []
        Just bs -> (bs:) <$> go

-- | Create an XML-RPC compliant HTTP request.
request :: URI -> URIAuth -> [(BS.ByteString, BS.ByteString)] -> Int64 -> IO Request
request uri auth usrHeaders len = buildRequest $ do
    http POST (BS.pack $ uriPath uri)
    setContentType "text/xml"
    setContentLength len

    case parseUserInfo auth of
      (Just user, Just pass) -> setAuthorizationBasic (BS.pack user) (BS.pack pass)
      _                      -> return ()

    mapM_ (uncurry setHeader) usrHeaders

    setHeader "User-Agent" userAgent

    where
      parseUserInfo info = let (u,pw) = break (==':') $ uriUserInfo info
                           in ( if null u then Nothing else Just u
                              , if null pw then Nothing else Just (tail pw))

--
-- Utility functions
--

maybeFail :: Monad m => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return
