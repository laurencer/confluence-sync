--
-- HTTP client for use with io-streams
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-imports #-}

module TestSuite where

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder (fromByteString,
                                                      toByteString)
import qualified Blaze.ByteString.Builder.Char8 as Builder (fromChar,
                                                            fromString)
import Control.Applicative
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, bracket, handleJust)
import Control.Monad (forM_, guard)
import Data.Aeson (FromJSON, ToJSON, Value (..), json, object, parseJSON,
                   toJSON, (.:), (.=))
import Data.Aeson.Encode.Pretty
import Data.Bits
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromJust)
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics hiding (Selector)
import Network.Socket (SockAddr (..))
import Network.URI (parseURI)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (Selector, anyException, shouldThrow)
import Test.HUnit

import Debug.Trace

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, parseTest)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Debug.Trace
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Debug as Streams

--
-- what we're actually testing
--

import MockServer (localPort)
import Network.Http.Client
import Network.Http.Connection (Connection (..))
import Network.Http.Inconvenience (HttpClientError (..),
                                   TooManyRedirects (..), splitURI)
import Network.Http.Internal (Request (..), Response (..),
                              composeRequestBytes, lookupHeader)
import Network.Http.ResponseParser (readDecimal, readResponseHeader)


localhost = S.pack ("localhost:" ++ show localPort)

suite :: Spec
suite = do
    describe "Opening a connection" $ do
        testConnectionHost

    describe "Request, when serialized" $ do
        testRequestLineFormat
        testRequestTermination
        testEnsureHostField
        testAcceptHeaderFormat
        testBasicAuthorizatonHeader

    describe "Parsing responses" $ do
        testResponseParser1
        testResponseParserMismatch
        testPaddedContentLength
--      testTrailingWhitespace
        testChunkedEncoding
        testContentLength
        testDevoidOfContent
        testCompressedResponse
        testRepeatedResponseHeaders

    describe "Expectation handling" $ do
        testExpectationContinue

    describe "Convenience API" $ do
        testPutChunks
        testPostChunks
        testPostWithForm
        testGetRedirects
        testSplitURI
        testGetLocalRedirects
        testGetFormatsRequest
        testExcessiveRedirects
        testGeneralHandler
        testEstablishConnection
        testParsingJson1
        testParsingJson2

    describe "Corner cases in protocol compliance" $ do
        testSendBodyFor PUT
        testSendBodyFor DELETE
        testSendBodyFor PATCH

testRequestTermination =
    it "terminates with a blank line" $ do
        c <- openConnection "localhost" localPort
        let q = buildRequest1 $ do
                    http GET "/time"
                    setAccept "text/plain"

        let e' = Builder.toByteString $ composeRequestBytes q "booga"
        let n = S.length e' - 4
        let (a',b') = S.splitAt n e'

        assertEqual "Termination not CRLF CRLF" "\r\n\r\n" b'
        assertBool "Must be only one blank line at end of headers"
            ('\n' /= S.last a')

        closeConnection c

testRequestLineFormat = do
    it "has a properly formatted request line" $ bracket
        (fakeConnection)
        (return)
        (\c -> do
            let q = buildRequest1 $ do
                        http GET "/time"

            let e' = Builder.toByteString $ composeRequestBytes q (cHost c)
            let l' = S.takeWhile (/= '\r') e'

            assertEqual "Invalid HTTP request line" "GET /time HTTP/1.1" l')

    it "handles empty request path" $ bracket
        (fakeConnection)
        (return)
        (\c -> do
            let q = buildRequest1 $ do
                        http GET ""

            let e' = Builder.toByteString $ composeRequestBytes q (cHost c)
            let l' = S.takeWhile (/= '\r') e'

            assertEqual "Invalid HTTP request line" "GET / HTTP/1.1" l')


fakeConnection :: IO Connection
fakeConnection = do
    i <- Streams.nullInput
    o <- Streams.nullOutput
    c <- makeConnection "www.example.com" (return ()) o i
    return c


testAcceptHeaderFormat =
    it "properly formats Accept header" $ do
        let q = buildRequest1 $ do
                    setAccept' [("text/html", 1),("*/*", 0.0)]

        let h = qHeaders q
        let (Just a) = lookupHeader h "Accept"
        assertEqual "Failed to format header" "text/html; q=1.0, */*; q=0.0" a

testBasicAuthorizatonHeader =
    it "properly formats Authorization header" $ do
        let q = buildRequest1 $ do
                    setAuthorizationBasic "Aladdin" "open sesame"

        let h = qHeaders q
        let (Just a) = lookupHeader h "Authorization"
        assertEqual "Failed to format header" "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" a


testConnectionHost = do
    it "properly caches hostname and port" $ do
        bracket (openConnection "localhost" localPort)
                closeConnection
                (\c -> do
                     let h' = cHost c
                     assertEqual "Host value needs to be name, not IP address"
                                 expected h')
  where
    expected = S.pack $ "localhost:" ++ show localPort


{-
    Incidentally, Host is *not* stored in the Headers map, but is a field
    of the Request object.
-}
testEnsureHostField =
    it "has a properly formatted Host header" $ do
        let q1 = buildRequest1 $ do
                    http GET "/hello.txt"

        let h1 = qHost q1
        assertEqual "Incorrect Host header" Nothing h1

        let q2 = buildRequest1 $ do
                    http GET "/hello.txt"
                    setHostname "other.example.com" 80

        let h2 = qHost q2
        assertEqual "Incorrect Host header" (Just "other.example.com") h2

        let q3 = buildRequest1 $ do
                    http GET "/hello.txt"
                    setHostname "other.example.com" 54321

        let h3 = qHost q3
        assertEqual "Incorrect Host header" (Just "other.example.com:54321") h3


testResponseParser1 =
    it "parses a simple 200 response" $ do
        p <- Streams.withFileAsInput "tests/example1.txt" (\i -> readResponseHeader i)

        assertEqual "Incorrect parse of response" 200 (getStatusCode p)
        return ()

testResponseParserMismatch =
    it "parses response when HTTP version doesn't match" $ do
        p <- Streams.withFileAsInput "tests/example3.txt" (\i -> readResponseHeader i)

        assertEqual "Incorrect parse of response" 200 (getStatusCode p)
        return ()

testPaddedContentLength =
    it "handles padded Content-Length" $ do
        p <- Streams.withFileAsInput "tests/example4.txt" (\i -> readResponseHeader i)

        let (Just len) = pContentLength p
        assertEqual "Should have trimmed in decimal conversion" 86 len

{-
    Presently inactive
-}
testTrailingWhitespace =
    it "where headers have trailing whitespace" $ do
        p <- Streams.withFileAsInput "tests/example4.txt" (\i -> readResponseHeader i)

        let (Just value) = getHeader p "Content-Length"
        assertEqual "Should have trimmed field value" "86" value

testChunkedEncoding =
    it "recognizes chunked transfer encoding and decodes" $ do
        c <- openConnection "localhost" localPort

        let q = buildRequest1 $ do
                    http GET "/time"

        sendRequest c q emptyBody
        receiveResponse c (\p i1 -> do
            let cm = getHeader p "Transfer-Encoding"
            assertEqual "Should be chunked encoding!" (Just "chunked") cm

            (i2, getCount) <- Streams.countInput i1
            Streams.skipToEof i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 29 len)


testContentLength = do
    it "recognzies fixed length message" $ do
        c <- openConnection "localhost" localPort

        let q = buildRequest1 $ do
                    http GET "/static/statler.jpg"

        sendRequest c q emptyBody

        receiveResponse c (\p i1 -> do
            let nm = getHeader p "Content-Length"
            assertMaybe "Should be a Content-Length header!" nm

            let n = read $ S.unpack $ fromJust nm :: Int
            assertEqual "Should be a fixed length message!" 4611 n

            (i2, getCount) <- Streams.countInput i1
            x' <- Streams.readExactly 4611 i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 4611 len
            assertBool "Incorrect length" (4611 == S.length x')

            end <- Streams.atEOF i2
            assertBool "Expected end of stream" end)

    it "doesn't choke if server neglects Content-Length" $ do
        p <- Streams.withFileAsInput "tests/example3.txt" (\i -> readResponseHeader i)

        assertEqual "Incorrect parse of response" 200 (getStatusCode p)
        assertEqual "Incorrect parse of response" Nothing (getHeader p "Content-Length")
        assertEqual "Should not have pContentLength" Nothing (pContentLength p)
        return ()

    it "reads body without Content-Length or Transfer-Encoding" $ do
        c <- fakeConnectionHttp10
        let q = buildRequest1 $ do
                    http GET "/fake"
        sendRequest c q emptyBody
        receiveResponse c (\_ i1 -> do
            (i2, getCount) <- Streams.countInput i1
            o <- Streams.nullOutput
            Streams.connect i2 o

            end <- Streams.atEOF i2
            assertBool "Expected end of stream" end

            len <- getCount
            assertEqual "Incorrect number of bytes read" 4611 len)

        return ()


fakeConnectionHttp10 :: IO Connection
fakeConnectionHttp10 = do
    x' <- S.readFile "tests/example3.txt"
    i <- Streams.fromByteString x'

    o <- Streams.nullOutput
    c <- makeConnection "bad.example.com" (return ()) o i
    return c


{-
    Corner case where servers responding 204 No Content are not required to
    transmit a Content-Length header; Snap *does* send one, so we can't test it
    in the MockServer, so fake it with example5.txt
-}

testDevoidOfContent = do
    it "handles 204 No Content response without Content-Length"
      $ timeout_ 2 $ do
        (c, mv) <- fakeConnectionNoContent
        let q = buildRequest1 $ do
                    http GET "/fake"
        sendRequest c q emptyBody
        receiveResponse c (\_ i1 -> do
            (i2, getCount) <- Streams.countInput i1
            o <- Streams.nullOutput
            Streams.connect i2 o

            end <- Streams.atEOF i2
            assertBool "Expected end of stream" end

            len <- getCount
            assertEqual "Incorrect number of bytes read" 0 len)
        putMVar mv ()
        return ()
  where
    secs :: Int
    secs = 10 ^ (6 :: Int)

    timeout_ :: Int -> IO a -> IO a
    timeout_ t m = timeout (t * secs) m >>= maybe (error "timeout") return


fakeConnectionNoContent :: IO (Connection, MVar ())
fakeConnectionNoContent = do
    x' <- S.readFile "tests/example5.txt"
    i1 <- Streams.fromByteString x'
    mv <- newEmptyMVar
    i2 <- Streams.makeInputStream $ blockOn mv
    i3 <- Streams.concatInputStreams [i1, i2]

    o <- Streams.nullOutput
    c <- makeConnection "worse.example.com" (return ()) o i3
    return (c, mv)
  where
    blockOn :: MVar () -> IO (Maybe ByteString)
    blockOn mv = takeMVar mv >> return Nothing


{-
    This had to change when we moved to an internal test server; seems
    Snap is doing something funny when gzipping and switching to chunked
    encoding no matter what I do.
-}
testCompressedResponse =
    it "recognizes gzip content encoding and decompresses" $ do
        c <- openConnection "localhost" localPort

        let q = buildRequest1 $ do
                    http GET "/static/hello.html"
                    setHeader "Accept-Encoding" "gzip"

        sendRequest c q emptyBody

        receiveResponse c (\p i -> do
            let nm = getHeader p "Content-Encoding"
            assertMaybe "Should be a Content-Encoding header!" nm
            assertEqual "Content-Encoding header should be 'gzip'!" (Just "gzip") nm

            (i2, getCount) <- Streams.countInput i
            x' <- Streams.readExactly 102 i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 102 len
            assertBool "Incorrect length" (102 == S.length x')

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end)

{-
    This isn't much of a test yet; we really need to test
    a) that 100 Continue was received b) that it was absorbed
    c) that body is correct size, and then d) 4xx and 5xx
    responses are propegated through.
-}

testExpectationContinue =
    it "sends expectation and handles 100 response" $ do
        c <- openConnection "localhost" localPort

        let q = buildRequest1 $ do
                    http PUT "/resource/x149"
                    setExpectContinue

        sendRequest c q (\o -> do
            Streams.write (Just (Builder.fromString "Hello world\n")) o)

        receiveResponse c (\p i -> do
            assertEqual "Incorrect status code" 201 (getStatusCode p)
            x' <- Streams.readExactly 12 i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            assertEqual "Incorrect body" "Hello world\n" x')

        closeConnection c


assertMaybe :: String -> Maybe a -> Assertion
assertMaybe prefix m0 =
    case m0 of
        Nothing -> assertFailure prefix
        Just _  -> assertBool "" True


testPutChunks =
    it "PUT correctly chunks known size entity body" $ do
        let url = S.concat ["http://", localhost, "/size"]

        put url "text/plain" body handler
      where
        body :: OutputStream Builder -> IO ()
        body o = do
            let x = mconcat $ replicate 33000 (Builder.fromChar 'x')
            Streams.write (Just x) o

        handler :: Response -> InputStream ByteString -> IO ()
        handler _ i = do
            (Just b') <- Streams.read i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            let size = readDecimal b' :: Int
            assertEqual "Should have replied with correct file size" 33000 size

testSendBodyFor meth =
    it ("Sends a request body for " ++ show meth) $ do
        c <- openConnection "localhost" localPort

        let q = buildRequest1 $ do
                    http meth "/size"
                    setContentType "text/plain"
                    setTransferEncoding

        sendRequest c q (\o -> do
            Streams.write (Just (Builder.fromString "a request")) o)

        receiveResponse c (\p i -> do
            assertEqual "Incorrect status code" 200 (getStatusCode p)
            (Just b') <- Streams.read i

            let size = readDecimal b' :: Int
            assertEqual "Should have received a request body" 9 size)

        closeConnection c

testPostChunks =
    it "POST correctly chunks a fileBody" $ do
        let url = S.concat ["http://", localhost, "/size"]

        post url "image/jpeg" (fileBody "tests/statler.jpg") handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler p i = do
            let code = getStatusCode p
            assertEqual "Expected 200 OK" 200 code

            (Just b') <- Streams.read i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            let size = readDecimal b' :: Int
            assertEqual "Should have replied with correct file size" 4611 size


testPostWithForm =
    it "POST with form data correctly encodes parameters" $ do
        let url = S.concat ["http://", localhost, "/postbox"]

        postForm url [ ("name", "Kermit")
                     , ("role", "St&gehand")
                     , ("country", Text.encodeUtf8 $ Text.pack "Nørway")
                     ] handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler p i = do
            let code = getStatusCode p
            assertEqual "Expected 201" 201 code

            b' <- Streams.readExactly 48 i

            end <- Streams.atEOF i
            assertBool "Expected end of stream" end

            assertEqual "Incorrect URL encoding"
                        "name=Kermit&role=St%26gehand&country=N%c3%b8rway"
                        b'


testGetRedirects =
    it "GET internal handler follows redirect on 307" $ do
        let url = S.concat ["http://", localhost, "/bounce"]

        get url handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler p i1 = do
            let code = getStatusCode p
            assertEqual "Should have been final code" 200 code

            (i2, getCount) <- Streams.countInput i1
            Streams.skipToEof i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 29 len


testGetLocalRedirects =
    it "GET internal handler follows local redirect on 307" $ do
        let url = S.concat ["http://", localhost, "/local"]

        get url handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler p i1 = do
            let code = getStatusCode p
            assertEqual "Should have been final code" 200 code

            (i2, getCount) <- Streams.countInput i1
            Streams.skipToEof i2

            len <- getCount
            assertEqual "Incorrect number of bytes read" 29 len


testSplitURI =
    it "check splitURI for local redirects" $ do
        let a1 = "http://asdf@ya.ru:8000/hello/?asd=asd&abc=2"
            r1 =                S.pack "/hello/?asd=asd&abc=2"
        assertEqual "Incorrect split uri 1" (S.pack a1) (splitURI (fromJust $ parseURI a1) r1)

        let a2 = "http://asdf@ya.ru:8000/again/?asd=asd&abc=2"
            r2 =                S.pack "/again/?asd=asd&abc=2"
        assertEqual "Incorrect split uri 2" (S.pack a2) (splitURI (fromJust $ parseURI a2) r2)

        let a3 = "http://ya.ru:8000/here/?asd=asd&abc=2"
            r3 =           S.pack "/here/?asd=asd&abc=2"
        assertEqual "Incorrect split uri 3" (S.pack a3) (splitURI (fromJust $ parseURI a3) r3)

        let a4 = "http://ya.ru/?asd=asd&abc=2#papa"
            r4 =      S.pack "/?asd=asd&abc=2#papa"
        assertEqual "Incorrect split uri 4" (S.pack a4) (splitURI (fromJust $ parseURI a4) r4)

        let a5 =            "http://ya.ru/?asd=asd&abc=2#papa"
            r5 = S.pack "http://google.ru/"
        assertEqual "Incorrect split uri 5" r5          (splitURI (fromJust $ parseURI a5) r5)


testGetFormatsRequest =
    it "GET includes a properly formatted request path" $ do
        let url = S.concat ["http://", localhost ]
        x' <- get url concatHandler'

        assertBool "Incorrect context path" (S.length x' > 0)

testExcessiveRedirects =
    it "too many redirects result in an exception" $ do
        let url = S.concat ["http://", localhost, "/loop"]

        get url handler `shouldThrow` tooManyRedirects
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler _ _ = do
            assertBool "Should have thrown exception before getting here" False

testRepeatedResponseHeaders =
    it "repeated response headers are properly concatonated" $ do
        let url = S.concat ["http://", localhost, "/cookies"]

        get url handler
      where
        handler :: Response -> InputStream ByteString -> IO ()
        handler r _ = do
            assertEqual "Invalid response headers" (Just "stone=diamond,metal=tungsten") (getHeader r "Set-Cookie")

{-
    From http://stackoverflow.com/questions/6147435/is-there-an-assertexception-in-any-of-the-haskell-test-frameworks
    because "although HUnit doesn't have this, it's easy to write your
    own". Uh huh. Surely there's an easier way to do this.
-}

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        _ <- action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)


testGeneralHandler =
    it "GET with general purpose handler throws exception on 404" $ do
        let url = S.concat ["http://", localhost, "/booga"]

        get url concatHandler' `shouldThrow` httpClientError 404


tooManyRedirects :: Selector TooManyRedirects
tooManyRedirects = const True

--              :: Int -> Selector HttpClientError
httpClientError :: Int -> HttpClientError -> Bool
httpClientError expected (HttpClientError actual _) = expected == actual



testEstablishConnection =
    it "public establish function behaves correctly" $ do
        let url = S.concat ["http://", localhost, "/static/statler.jpg"]

        x' <- withConnection (establishConnection url) $ (\c -> do
            let q = buildRequest1 $ do
                        http GET "/static/statler.jpg"
                    -- TODO be nice if we could replace that with 'url';
                    -- fix the routeRequests function in TestServer maybe?
            sendRequest c q emptyBody
            receiveResponse c concatHandler')

        let len = S.length x'
        assertEqual "Incorrect number of bytes read" 4611 len


testParsingJson1 =
    it "GET with JSON handler behaves" $ do
        let url = S.concat ["http://", localhost, "/static/data-eu-gdp.json"]

        x <- get url jsonHandler
        let (Object o) = x
        let (Just v) = Map.lookup "label" o
        let (String t) = v

        assertEqual "Incorrect response" "Europe (EU27)" t

testParsingJson2 =
    it "GET with JSON handler parses using Aeson" $ do
        let url = S.concat ["http://", localhost, "/static/data-jp-gdp.json"]

        x <- get url jsonHandler :: IO GrossDomesticProduct

        assertEqual "Incorrect response" "Japan" (gLabel x)
        assertEqual "Data not parsed as expected" 2008 (fst $ last $ gData x)
--      L.putStr $ encodePretty x


{-
    Go to the trouble to create a Haskell data type representing the JSON feed
    we're getting from the sample data files. The Generic trick doesn't work
    because data is a reserved word, of course.
-}

data GrossDomesticProduct = GrossDomesticProduct {
    gLabel :: Text,
    gData  :: [(Int, Double)]
} deriving (Show, Generic)

instance FromJSON GrossDomesticProduct where
    parseJSON (Object o)    = GrossDomesticProduct <$>
                                o .: "label" <*>
                                o .: "data"
    parseJSON _             = undefined


instance ToJSON GrossDomesticProduct where
    toJSON (GrossDomesticProduct l d) = object
                               ["label" .= l,
                                "data"  .= d]

