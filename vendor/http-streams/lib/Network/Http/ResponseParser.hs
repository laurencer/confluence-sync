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
-- Significant portions of this file were written while studying
-- the HTTP request parser implementation in the Snap Framework;
-- snap-core's src/Snap/Internal/Parsing.hs and snap-server's
-- src/Snap/Internal/Http/Parser.hs, and various utility functions
-- have been cloned from there.
--

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.Http.ResponseParser (
    readResponseHeader,
    readResponseBody,
    UnexpectedCompression(..),

        -- for testing
    readDecimal
) where

import Prelude hiding (take, takeWhile)

import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.ByteString.Char8
import Data.Bits (Bits (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.CaseInsensitive (mk)
import Data.Char (ord)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import System.IO.Streams (Generator, InputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Network.Http.Internal
import Network.Http.Utilities

{-
    The chunk size coming down from the server is somewhat arbitrary;
    it's really just an indication of how many bytes need to be read
    before the next size marker or end marker - neither of which has
    anything to do with streaming on our side. Instead, we'll feed
    bytes into our InputStream at an appropriate intermediate size.
-}
__BITE_SIZE__ :: Int
__BITE_SIZE__ = 32 * 1024


{-
    Process the reply from the server up to the end of the headers as
    deliniated by a blank line.
-}
readResponseHeader :: InputStream ByteString -> IO Response
readResponseHeader i = do
    (sc,sm) <- Streams.parseFromStream parseStatusLine i

    hs <- readHeaderFields i

    let h  = buildHeaders hs
    let te = case lookupHeader h "Transfer-Encoding" of
            Just x' -> if mk x' == "chunked"
                        then Chunked
                        else None
            Nothing -> None

    let ce = case lookupHeader h "Content-Encoding" of
            Just x' -> if mk x' == "gzip"
                        then Gzip
                        else Identity
            Nothing -> Identity

    let nm = case lookupHeader h "Content-Length" of
            Just x' -> Just (readDecimal x' :: Int64)
            Nothing -> case sc of
                        204 -> Just 0
                        304 -> Just 0
                        100 -> Just 0
                        _   -> Nothing

    return Response {
        pStatusCode = sc,
        pStatusMsg = sm,
        pTransferEncoding = te,
        pContentEncoding = ce,
        pContentLength = nm,
        pHeaders = h
    }


parseStatusLine :: Parser (Int,ByteString)
parseStatusLine = do
    sc <- string "HTTP/1." *> satisfy version *> char ' ' *> decimal <* char ' '
    sm <- takeTill (== '\r') <* crlf
    return (sc,sm)
  where
    version c = c == '1' || c == '0'


crlf :: Parser ByteString
crlf = string "\r\n"


---------------------------------------------------------------------

{-
    Switch on the encoding and compression headers, wrapping the raw
    InputStream to present the entity body's actual bytes.
-}
readResponseBody :: Response -> InputStream ByteString -> IO (InputStream ByteString)
readResponseBody p i1 = do

    i2 <- case t of
        None        -> case l of
                        Just n  -> readFixedLengthBody i1 n
                        Nothing -> readUnlimitedBody i1
        Chunked     -> readChunkedBody i1

    i3 <- case c of
        Identity    -> return i2
        Gzip        -> readCompressedBody i2
        Deflate     -> throwIO (UnexpectedCompression $ show c)

    return i3
  where
    t = pTransferEncoding p
    c = pContentEncoding p
    l = pContentLength p


readDecimal :: (Enum α, Num α, Bits α) => ByteString -> α
readDecimal str' =
    S.foldl' f 0 x'
  where
    f !cnt !i = cnt * 10 + digitToInt i

    x' = head $ S.words str'

    {-# INLINE digitToInt #-}
    digitToInt :: (Enum α, Num α, Bits α) => Char -> α
    digitToInt c | c >= '0' && c <= '9' = toEnum $! ord c - ord '0'
                 | otherwise = error $ "'" ++ [c] ++ "' is not an ascii digit"
{-# INLINE readDecimal #-}

data UnexpectedCompression = UnexpectedCompression String
        deriving (Typeable, Show)

instance Exception UnexpectedCompression


---------------------------------------------------------------------

{-
    Process a response body in chunked transfer encoding, taking the
    resultant bytes and reproducing them as an InputStream
-}
readChunkedBody :: InputStream ByteString -> IO (InputStream ByteString)
readChunkedBody i1 = do
    i2 <- Streams.fromGenerator (consumeChunks i1)
    return i2


{-
    For a response body in chunked transfer encoding, iterate over
    the individual chunks, reading the size parameter, then
    looping over that chunk in bites of at most __BYTE_SIZE__,
    yielding them to the receiveResponse InputStream accordingly.
-}
consumeChunks :: InputStream ByteString -> Generator ByteString ()
consumeChunks i1 = do
    !n <- parseSize

    if n > 0
        then do
            -- read one or more bites, then loop to next chunk
            go n
            skipCRLF
            consumeChunks i1
        else do
            -- skip "trailers" and consume final CRLF
            skipEnd

  where
    go 0 = return ()
    go !n = do
        (!x',!r) <- liftIO $ readN n i1
        Streams.yield x'
        go r

    parseSize = do
        n <- liftIO $ Streams.parseFromStream transferChunkSize i1
        return n

    skipEnd = do
        liftIO $ do
            _ <- readHeaderFields i1
            return ()

    skipCRLF = do
        liftIO $ do
            _ <- Streams.parseFromStream crlf i1
            return ()

{-
    Read the specified number of bytes up to a maximum of __BITE_SIZE__,
    returning a resultant ByteString and the number of bytes remaining.
-}

readN :: Int -> InputStream ByteString -> IO (ByteString, Int)
readN n i1 = do
    !x' <- Streams.readExactly p i1
    return (x', r)
  where
    !d = n - size

    !p = if d > 0
        then size
        else n

    !r = if d > 0
        then d
        else 0

    size = __BITE_SIZE__


transferChunkSize :: Parser (Int)
transferChunkSize = do
    !n <- hexadecimal
    void (takeTill (== '\r'))
    void crlf
    return n


---------------------------------------------------------------------

{-
    This has the rather crucial side effect of terminating the stream
    after the requested number of bytes. Otherwise, code handling
    responses waits on more input until an HTTP timeout occurs.
-}
readFixedLengthBody :: InputStream ByteString -> Int64 -> IO (InputStream ByteString)
readFixedLengthBody i1 n = do
    i2 <- Streams.takeBytes n i1
    return i2

{-
    On the other hand, there is the (predominently HTTP/1.0) case
    where there is no content length sent and no chunking, with the
    result that only the connection closing marks the end of the
    response body.
-}
readUnlimitedBody :: InputStream ByteString -> IO (InputStream ByteString)
readUnlimitedBody i1 = do
    return i1


---------------------------------------------------------------------

readCompressedBody :: InputStream ByteString -> IO (InputStream ByteString)
readCompressedBody i1 = do
    i2 <- Streams.gunzip i1
    return i2
