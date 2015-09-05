-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Server
-- Copyright   :  (c) Bjorn Bringert 2003
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- This module contains the server functionality of XML-RPC.
-- The XML-RPC specifcation is available at <http://www.xmlrpc.com/spec>.
--
-- A simple CGI-based XML-RPC server application:
--
-- > import Network.XmlRpc.Server
-- >
-- > add :: Int -> Int -> IO Int
-- > add x y = return (x + y)
-- >
-- > main = cgiXmlRpcServer [("examples.add", fun add)]
-----------------------------------------------------------------------------

module Network.XmlRpc.Server
    (
     XmlRpcMethod, ServerResult,
     fun,
     handleCall, methods, cgiXmlRpcServer,
    ) where

import           Network.XmlRpc.Internals

import qualified Codec.Binary.UTF8.String   as U
import           Control.Exception
import           Control.Monad.Except
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           System.IO

serverName :: String
serverName = "Haskell XmlRpcServer/0.1"

--
-- API
--

type ServerResult = Err IO MethodResponse

type Signature = ([Type], Type)

-- | The type of XML-RPC methods on the server.
type XmlRpcMethod = (MethodCall -> ServerResult, Signature)

showException :: SomeException -> String
showException = show

handleIO :: IO a -> Err IO a
handleIO io = lift (try io) >>= either (fail . showException) return


--
-- Converting Haskell functions to XML-RPC methods
--

-- | Turns any function
--   @(XmlRpcType t1, ..., XmlRpcType tn, XmlRpcType r) =>
--   t1 -> ... -> tn -> IO r@
--   into an 'XmlRpcMethod'
fun :: XmlRpcFun a => a -> XmlRpcMethod
fun f = (toFun f, sig f)

class XmlRpcFun a where
    toFun :: a -> MethodCall -> ServerResult
    sig :: a -> Signature

instance XmlRpcType a => XmlRpcFun (IO a) where
    toFun x (MethodCall _ []) = do
			      v <- handleIO x
			      return (Return (toValue v))
    toFun _ _ = fail "Too many arguments"
    sig x = ([], getType (mType x))

instance (XmlRpcType a, XmlRpcFun b) => XmlRpcFun (a -> b) where
    toFun f (MethodCall n (x:xs)) = do
				  v <- fromValue x
				  toFun (f v) (MethodCall n xs)
    toFun _ _ = fail "Too few arguments"
    sig f = let (a,b) = funType f
                (as, r) = sig b
             in (getType a : as, r)

mType :: m a -> a
mType _ = undefined

funType :: (a -> b) -> (a, b)
funType _ = (undefined, undefined)

-- FIXME: always returns error code 0
errorToResponse :: ServerResult -> IO MethodResponse
errorToResponse = handleError (return . Fault 0)


-- | Reads a method call from a string, uses the supplied method
--   to generate a response and returns that response as a string
handleCall :: (MethodCall -> ServerResult) -> String -> IO ByteString
handleCall f str = do resp <- errorToResponse (parseCall str >>= f)
		      return (renderResponse resp)

-- | An XmlRpcMethod that looks up the method name in a table
--   and uses that method to handle the call.
methods :: [(String,XmlRpcMethod)] -> MethodCall -> ServerResult
methods t c@(MethodCall name _) =
    do
    (method,_) <- maybeToM ("Unknown method: " ++ name) (lookup name t)
    method c


-- | A server with introspection support
server :: [(String,XmlRpcMethod)] -> String -> IO ByteString
server t = handleCall (methods (addIntrospection t))



--
-- Introspection
--

addIntrospection :: [(String,XmlRpcMethod)] -> [(String,XmlRpcMethod)]
addIntrospection t = t'
	where t' = ("system.listMethods", fun (listMethods t')) :
		   ("system.methodSignature", fun (methodSignature t')) :
		   ("system.methodHelp", fun (methodHelp t')) : t

listMethods :: [(String,XmlRpcMethod)] -> IO [String]
listMethods t = return (fst (unzip t))

methodSignature :: [(String,XmlRpcMethod)] -> String -> IO [[String]]
methodSignature t name =
    do
    (_,(as,r)) <- maybeToM ("Unknown method: " ++ name) (lookup name t)
    return [map show (r:as)]

methodHelp :: [(String,XmlRpcMethod)] -> String -> IO String
methodHelp t name =
    do
    method <- maybeToM ("Unknown method: " ++ name) (lookup name t)
    return (help method)

-- FIXME: implement
help :: XmlRpcMethod -> String
help _ = ""


--
-- CGI server
--

-- | A CGI-based XML-RPC server. Reads a request from standard input
--   and writes some HTTP headers (Content-Type and Content-Length),
--   followed by the response to standard output. Supports
--   introspection.
cgiXmlRpcServer :: [(String,XmlRpcMethod)] -> IO ()
cgiXmlRpcServer ms =
    do
    hSetBinaryMode stdin True
    hSetBinaryMode stdout True
    input <- U.decodeString `fmap` getContents
    --output <- U.encodeString `fmap` server ms input
    output <- server ms input
    putStr ("Server: " ++ serverName ++ crlf)
    putStr ("Content-Type: text/xml" ++ crlf)
    putStr ("Content-Length: " ++ show (B.length output) ++ crlf)
    putStr crlf
    B.putStr output
	where crlf = "\r\n"
