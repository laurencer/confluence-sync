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

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse    #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Http.Connection (
    Connection(..),
        -- constructors only for testing
    makeConnection,
    withConnection,
    openConnection,
    openConnectionSSL,
    openConnectionUnix,
    closeConnection,
    getHostname,
    getRequestHeaders,
    getHeadersFull,
    sendRequest,
    receiveResponse,
    receiveResponseRaw,
    UnexpectedCompression,
    emptyBody,
    fileBody,
    inputStreamBody,
    debugHandler,
    concatHandler
) where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (flush, fromByteString,
                                                      toByteString)
import qualified Blaze.ByteString.Builder.HTTP as Builder (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Network.Socket
import OpenSSL (withOpenSSL)
import OpenSSL.Session (SSL, SSLContext)
import qualified OpenSSL.Session as SSL
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.SSL as Streams hiding (connect)

import Network.Socket.Options (setRecvTimeout, setSendTimeout)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mappend, mempty)
#endif

import Network.Http.Internal
import Network.Http.ResponseParser

--
-- | A connection to a web server.
--
data Connection
    = Connection {
        cHost  :: ByteString,
            -- ^ will be used as the Host: header in the HTTP request.
        cClose :: IO (),
            -- ^ called when the connection should be closed.
        cOut   :: OutputStream Builder,
        cIn    :: InputStream ByteString
    }

instance Show Connection where
    show c =    {-# SCC "Connection.show" #-}
        concat
           ["Host: ",
             S.unpack $ cHost c,
             "\n"]


--
-- | Create a raw Connection object from the given parts. This is
-- primarily of use when teseting, for example:
--
-- > fakeConnection :: IO Connection
-- > fakeConnection = do
-- >     o  <- Streams.nullOutput
-- >     i  <- Streams.nullInput
-- >     c  <- makeConnection "www.example.com" (return()) o i
-- >     return c
--
-- is an idiom we use frequently in testing and benchmarking, usually
-- replacing the InputStream with something like:
--
-- >     x' <- S.readFile "properly-formatted-response.txt"
-- >     i  <- Streams.fromByteString x'
--
-- If you're going to do that, keep in mind that you /must/ have CR-LF
-- pairs after each header line and between the header and body to
-- be compliant with the HTTP protocol; otherwise, parsers will
-- reject your message.
--
makeConnection
    :: ByteString
    -- ^ will be used as the @Host:@ header in the HTTP request.
    -> IO ()
    -- ^ an action to be called when the connection is terminated.
    -> OutputStream ByteString
    -- ^ write end of the HTTP client-server connection.
    -> InputStream ByteString
    -- ^ read end of the HTTP client-server connection.
    -> IO Connection
makeConnection h c o1 i = do
    o2 <- Streams.builderStream o1
    return $! Connection h c o2 i


--
-- | Given an @IO@ action producing a 'Connection', and a computation
-- that needs one, runs the computation, cleaning up the
-- @Connection@ afterwards.
--
-- >     x <- withConnection (openConnection "s3.example.com" 80) $ (\c -> do
-- >         let q = buildRequest1 $ do
-- >             http GET "/bucket42/object/149"
-- >         sendRequest c q emptyBody
-- >         ...
-- >         return "blah")
--
-- which can make the code making an HTTP request a lot more
-- straight-forward.
--
-- Wraps @Control.Exception@'s 'Control.Exception.bracket'.
--
withConnection :: IO Connection -> (Connection -> IO γ) -> IO γ
withConnection mkC =
    bracket mkC closeConnection


--
-- | In order to make a request you first establish the TCP
-- connection to the server over which to send it.
--
-- Ordinarily you would supply the host part of the URL here and it will
-- be used as the value of the HTTP 1.1 @Host:@ field. However, you can
-- specify any server name or IP addresss and set the @Host:@ value
-- later with 'Network.Http.Client.setHostname' when building the
-- request.
--
-- Usage is as follows:
--
-- >     c <- openConnection "localhost" 80
-- >     ...
-- >     closeConnection c
--
-- More likely, you'll use 'withConnection' to wrap the call in order
-- to ensure finalization.
--
-- HTTP pipelining is supported; you can reuse the connection to a
-- web server, but it's up to you to ensure you match the number of
-- requests sent to the number of responses read, and to process those
-- responses in order. This is all assuming that the /server/ supports
-- pipelining; be warned that not all do. Web browsers go to
-- extraordinary lengths to probe this; you probably only want to do
-- pipelining under controlled conditions. Otherwise just open a new
-- connection for subsequent requests.
--
openConnection :: Hostname -> Port -> IO Connection
openConnection h1' p = do
    is <- getAddrInfo (Just hints) (Just h1) (Just $ show p)
    let addr = head is
    let a = addrAddress addr
    s <- socket (addrFamily addr) Stream defaultProtocol

    let timeoutMicros = 60000000 -- 60 seconds
    setRecvTimeout s timeoutMicros
    setSendTimeout s timeoutMicros

    connect s a
    (i,o1) <- Streams.socketToStreams s

    o2 <- Streams.builderStream o1

    return Connection {
        cHost  = h2',
        cClose = close s,
        cOut   = o2,
        cIn    = i
    }
  where
    hints = defaultHints {
        addrFlags = [AI_ADDRCONFIG, AI_NUMERICSERV],
        addrSocketType = Stream
    }
    h2' = if p == 80
        then h1'
        else S.concat [ h1', ":", S.pack $ show p ]
    h1  = S.unpack h1'

--
-- | Open a secure connection to a web server.
--
-- > import OpenSSL (withOpenSSL)
-- >
-- > main :: IO ()
-- > main = do
-- >     ctx <- baselineContextSSL
-- >     c <- openConnectionSSL ctx "api.github.com" 443
-- >     ...
-- >     closeConnection c
--
-- If you want to tune the parameters used in making SSL connections,
-- manually specify certificates, etc, then setup your own context:
--
-- > import OpenSSL.Session (SSLContext)
-- > import qualified OpenSSL.Session as SSL
-- >
-- >     ...
-- >     ctx <- SSL.context
-- >     ...
--
-- See "OpenSSL.Session".
--
-- Crypto is as provided by the system @openssl@ library, as wrapped
-- by the @HsOpenSSL@ package and @openssl-streams@.
--
-- /There is no longer a need to call @withOpenSSL@ explicitly; the
-- initialization is invoked once per process for you/
--
openConnectionSSL :: SSLContext -> Hostname -> Port -> IO Connection
openConnectionSSL ctx h1' p = withOpenSSL $ do
    is <- getAddrInfo Nothing (Just h1) (Just $ show p)

    let a = addrAddress $ head is
        f = addrFamily $ head is
    s <- socket f Stream defaultProtocol

    let timeoutMicros = 60000000 -- 60 seconds
    setRecvTimeout s timeoutMicros
    setSendTimeout s timeoutMicros

    connect s a

    ssl <- SSL.connection ctx s
    SSL.connect ssl

    (i,o1) <- Streams.sslToStreams ssl

    o2 <- Streams.builderStream o1

    return Connection {
        cHost  = h2',
        cClose = closeSSL s ssl,
        cOut   = o2,
        cIn    = i
    }
  where
    h2' :: ByteString
    h2' = if p == 443
        then h1'
        else S.concat [ h1', ":", S.pack $ show p ]
    h1  = S.unpack h1'

closeSSL :: Socket -> SSL -> IO ()
closeSSL s ssl = do
    SSL.shutdown ssl SSL.Unidirectional
    close s

--
-- | Open a connection to a Unix domain socket.
--
-- > main :: IO ()
-- > main = do
-- >     c <- openConnectionUnix "/var/run/docker.sock"
-- >     ...
-- >     closeConnection c
--
openConnectionUnix :: FilePath -> IO Connection
openConnectionUnix path = do
    let a = SockAddrUnix path
    s <- socket AF_UNIX Stream defaultProtocol

    connect s a
    (i,o1) <- Streams.socketToStreams s

    o2 <- Streams.builderStream o1

    return Connection {
        cHost  = path',
        cClose = close s,
        cOut   = o2,
        cIn    = i
    }
  where
    path'  = S.pack path

--
-- | Having composed a 'Request' object with the headers and metadata for
-- this connection, you can now send the request to the server, along
-- with the entity body, if there is one. For the rather common case of
-- HTTP requests like 'GET' that don't send data, use 'emptyBody' as the
-- output stream:
--
-- >     sendRequest c q emptyBody
--
-- For 'PUT' and 'POST' requests, you can use 'fileBody' or
-- 'inputStreamBody' to send content to the server, or you can work with
-- the @io-streams@ API directly:
--
-- >     sendRequest c q (\o ->
-- >         Streams.write (Just (Builder.fromString "Hello World\n")) o)
--
{-
    I would like to enforce the constraints on the Empty and Static
    cases shown here, but those functions take OutputStream ByteString,
    and we are of course working in OutputStream Builder by that point.
-}
sendRequest :: Connection -> Request -> (OutputStream Builder -> IO α) -> IO α
sendRequest c q handler = do
    -- write the headers

    Streams.write (Just msg) o2

    -- deal with the expect-continue mess

    e2 <- case t of
        Normal -> do
            return e

        Continue -> do
            Streams.write (Just Builder.flush) o2

            p  <- readResponseHeader i

            case getStatusCode p of
                100 -> do
                        -- ok to send
                        return e
                _   -> do
                        -- put the response back
                        Streams.unRead (rsp p) i
                        return Empty

    -- write the body, if there is one

    x <- case e2 of
        Empty -> do
            o3 <- Streams.nullOutput
            y <- handler o3
            return y

        Chunking    -> do
            o3 <- Streams.contramap Builder.chunkedTransferEncoding o2
            y  <- handler o3
            Streams.write (Just Builder.chunkedTransferTerminator) o2
            return y

        (Static _) -> do
--          o3 <- Streams.giveBytes (fromIntegral n :: Int64) o2
            y  <- handler o2
            return y


    -- push the stream out by flushing the output buffers

    Streams.write (Just Builder.flush) o2

    return x

  where
    o2 = cOut c
    e = qBody q
    t = qExpect q
    msg = composeRequestBytes q h'
    h' = cHost c
    i = cIn c
    rsp p = Builder.toByteString $ composeResponseBytes p


--
-- | Get the virtual hostname that will be used as the @Host:@ header in
-- the HTTP 1.1 request. Per RFC 2616 § 14.23, this will be of the form
-- @hostname:port@ if the port number is other than the default, ie 80
-- for HTTP.
--
getHostname :: Connection -> Request -> ByteString
getHostname c q =
    case qHost q of
        Just h' -> h'
        Nothing -> cHost c


{-# DEPRECATED getRequestHeaders "use retrieveHeaders . getHeadersFull instead" #-}
getRequestHeaders :: Connection -> Request -> [(ByteString, ByteString)]
getRequestHeaders c q =
    ("Host", getHostname c q) : kvs
  where
    h = qHeaders q
    kvs = retrieveHeaders h

--
-- | Get the headers that will be sent with this request. You likely won't
-- need this but there are some corner cases where people need to make
-- calculations based on all the headers before they go out over the wire.
--
-- If you'd like the request headers as an association list, import the header
-- functions:
--
-- > import Network.Http.Types
--
-- then use 'Network.Http.Types.retreiveHeaders' as follows:
--
-- >>> let kvs = retreiveHeaders $ getHeadersFull c q
-- >>> :t kvs
-- :: [(ByteString, ByteString)]
--
getHeadersFull :: Connection -> Request -> Headers
getHeadersFull c q =
    h'
  where
    h  = qHeaders q
    h' = updateHeader h "Host" (getHostname c q)

--
-- | Handle the response coming back from the server. This function
-- hands control to a handler function you supply, passing you the
-- 'Response' object with the response headers and an 'InputStream'
-- containing the entity body.
--
-- For example, if you just wanted to print the first chunk of the
-- content from the server:
--
-- >     receiveResponse c (\p i -> do
-- >         m <- Streams.read i
-- >         case m of
-- >             Just bytes -> putStr bytes
-- >             Nothing    -> return ())
--
-- Obviously, you can do more sophisticated things with the
-- 'InputStream', which is the whole point of having an @io-streams@
-- based HTTP client library.
--
-- The final value from the handler function is the return value of
-- @receiveResponse@, if you need it.
--
-- Throws 'UnexpectedCompression' if it doesn't know how to handle the
-- compression format used in the response.
--
{-
    The reponse body coming from the server MUST be fully read, even
    if (especially if) the users's handler doesn't consume it all.
    This is necessary to maintain the HTTP protocol invariants;
    otherwise pipelining would not work. It's not entirely clear
    *which* InputStream is being drained here; the underlying
    InputStream ByteString in Connection remains unconsumed beyond the
    threshold of the current response, which is exactly what we need.
-}
receiveResponse :: Connection -> (Response -> InputStream ByteString -> IO β) -> IO β
receiveResponse c handler = do
    p  <- readResponseHeader i
    i' <- readResponseBody p i

    x  <- handler p i'

    Streams.skipToEof i'

    return x
  where
    i = cIn c

--
-- | This is a specialized variant of 'receiveResponse' that /explicitly/ does
-- not handle the content encoding of the response body stream (it will not
-- decompress anything). Unless you really want the raw gzipped content coming
-- down from the server, use @receiveResponse@.
--
{-
    See notes at receiveResponse.
-}
receiveResponseRaw :: Connection -> (Response -> InputStream ByteString -> IO β) -> IO β
receiveResponseRaw c handler = do
    p  <- readResponseHeader i
    let p' = p {
        pContentEncoding = Identity
    }

    i' <- readResponseBody p' i

    x  <- handler p i'

    Streams.skipToEof i'

    return x
  where
    i = cIn c


--
-- | Use this for the common case of the HTTP methods that only send
-- headers and which have no entity body, i.e. 'GET' requests.
--
emptyBody :: OutputStream Builder -> IO ()
emptyBody _ = return ()


--
-- | Specify a local file to be sent to the server as the body of the
-- request.
--
-- You use this partially applied:
--
-- >     sendRequest c q (fileBody "/etc/passwd")
--
-- Note that the type of @(fileBody \"\/path\/to\/file\")@ is just what
-- you need for the third argument to 'sendRequest', namely
--
-- >>> :t filePath "hello.txt"
-- :: OutputStream Builder -> IO ()
--
{-
    Relies on Streams.withFileAsInput generating (very) large chunks [which it
    does]. A more efficient way to do this would be interesting.
-}
fileBody :: FilePath -> OutputStream Builder -> IO ()
fileBody p o = do
    Streams.withFileAsInput p (\i -> inputStreamBody i o)


--
-- | Read from a pre-existing 'InputStream' and pipe that through to the
-- connection to the server. This is useful in the general case where
-- something else has handed you stream to read from and you want to use
-- it as the entity body for the request.
--
-- You use this partially applied:
--
-- >     i <- getStreamFromVault                    -- magic, clearly
-- >     sendRequest c q (inputStreamBody i)
--
-- This function maps "Builder.fromByteString" over the input, which will
-- be efficient if the ByteString chunks are large.
--
{-
    Note that this has to be 'supply' and not 'connect' as we do not
    want the end of stream to prematurely terminate the chunked encoding
    pipeline!
-}
inputStreamBody :: InputStream ByteString -> OutputStream Builder -> IO ()
inputStreamBody i1 o = do
    i2 <- Streams.map Builder.fromByteString i1
    Streams.supply i2 o


--
-- | Print the response headers and response body to @stdout@. You can
-- use this with 'receiveResponse' or one of the convenience functions
-- when testing. For example, doing:
--
-- >     c <- openConnection "kernel.operationaldynamics.com" 58080
-- >
-- >     let q = buildRequest1 $ do
-- >                 http GET "/time"
-- >
-- >     sendRequest c q emptyBody
-- >
-- >     receiveResponse c debugHandler
--
-- would print out:
--
-- > HTTP/1.1 200 OK
-- > Transfer-Encoding: chunked
-- > Content-Type: text/plain
-- > Vary: Accept-Encoding
-- > Server: Snap/0.9.2.4
-- > Content-Encoding: gzip
-- > Date: Mon, 21 Jan 2013 06:13:37 GMT
-- >
-- > Mon 21 Jan 13, 06:13:37.303Z
--
-- or thereabouts.
--
debugHandler :: Response -> InputStream ByteString -> IO ()
debugHandler p i = do
    S.putStr $ S.filter (/= '\r') $ Builder.toByteString $ composeResponseBytes p
    Streams.connect i stdout


--
-- | Sometimes you just want the entire response body as a single blob.
-- This function concatonates all the bytes from the response into a
-- ByteString. If using the main @http-streams@ API, you would use it
-- as follows:
--
-- >    ...
-- >    x' <- receiveResponse c concatHandler
-- >    ...
--
-- The methods in the convenience API all take a function to handle the
-- response; this function is passed directly to the 'receiveResponse'
-- call underlying the request. Thus this utility function can be used
-- for 'get' as well:
--
-- >    x' <- get "http://www.example.com/document.txt" concatHandler
--
-- Either way, the usual caveats about allocating a
-- single object from streaming I/O apply: do not use this if you are
-- not absolutely certain that the response body will fit in a
-- reasonable amount of memory.
--
-- Note that this function makes no discrimination based on the
-- response's HTTP status code. You're almost certainly better off
-- writing your own handler function.
--
{-
    I'd welcome a better name for this function.
-}
concatHandler :: Response -> InputStream ByteString -> IO ByteString
concatHandler _ i1 = do
    i2 <- Streams.map Builder.fromByteString i1
    x <- Streams.fold mappend mempty i2
    return $ Builder.toByteString x


--
-- | Shutdown the connection. You need to call this release the
-- underlying socket file descriptor and related network resources. To
-- do so reliably, use this in conjunction with 'openConnection' in a
-- call to 'Control.Exception.bracket':
--
-- > --
-- > -- Make connection, cleaning up afterward
-- > --
-- >
-- > foo :: IO ByteString
-- > foo = bracket
-- >    (openConnection "localhost" 80)
-- >    (closeConnection)
-- >    (doStuff)
-- >
-- > --
-- > -- Actually use Connection to send Request and receive Response
-- > --
-- >
-- > doStuff :: Connection -> IO ByteString
--
-- or, just use 'withConnection'.
--
-- While returning a ByteString is probably the most common use case,
-- you could conceivably do more processing of the response in 'doStuff'
-- and have it and 'foo' return a different type.
--
closeConnection :: Connection -> IO ()
closeConnection c = cClose c
