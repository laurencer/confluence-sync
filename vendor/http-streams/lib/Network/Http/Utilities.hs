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
-- This file is essentially a clone of Snap.Internal.Parsing,
-- the HTTP request parser implementation in the Snap Framework;
-- snap-core's src/Snap/Internal/Parsing.hs and snap-server's
-- src/Snap/Internal/Http/Parser.hs, copied here to specialize
-- it to Response parsing. This code replaces the attoparsec
-- based implementation formerly in ResponseParser, but is
-- kept separate to aid syncing changes from snap-core as they
-- become available.
--

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE UnboxedTuples      #-}

module Network.Http.Utilities (
    readResponseLine,
    readHeaderFields
) where

------------------------------------------------------------------------------
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Bits
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Internal (ByteString, w2c)
import qualified Data.ByteString.Unsafe as S
import Data.Char hiding (digitToInt, isDigit, isSpace)
import GHC.Exts (Int (..), Int#, (+#))
import Prelude hiding (head, take, takeWhile)
import System.IO.Streams (InputStream)
import qualified System.IO.Streams as Streams
----------------------------------------------------------------------------

import Network.Http.Types

------------------------------------------------------------------------------

{-
    This is vestigial; originally it was the Request parsing
    code in Snap. Keeping it here until we can use if for
    response parsing.
-}
parseRequest :: InputStream ByteString -> IO (Maybe Request)
parseRequest input = do
    eof <- Streams.atEOF input
    if eof
      then return Nothing
      else do
        line <- readResponseLine input
        let (!mStr,!s)      = bSp line
        let (!uri, !vStr)   = bSp s
        let !version        = pVer vStr :: (Int,Int)

--      hdrs    <- readHeaderFields input
        return $! Nothing

  where

    pVer s = if "HTTP/" `S.isPrefixOf` s
               then pVers (S.unsafeDrop 5 s)
               else (1, 0)

    bSp   = splitCh ' '

    pVers s = (c, d)
      where
        (!a, !b)   = splitCh '.' s
        !c         = unsafeFromNat a
        !d         = unsafeFromNat b


{-
    Read a single line of an HTTP response.
-}
readResponseLine :: InputStream ByteString -> IO ByteString
readResponseLine input = go []
  where
    throwNoCRLF =
        throwIO $
        HttpParseException "parse error: expected line ending in crlf"

    throwBadCRLF =
        throwIO $
        HttpParseException "parse error: got cr without subsequent lf"

    go !l = do
        !mb <- Streams.read input
        !s  <- maybe throwNoCRLF return mb

        case findCRLF s of
            FoundCRLF idx# -> foundCRLF l s idx#
            NoCR           -> noCRLF l s
            LastIsCR idx#  -> lastIsCR l s idx#
            _              -> throwBadCRLF

    foundCRLF l s idx# = do
        let !i1 = (I# idx#)
        let !i2 = (I# (idx# +# 2#))
        let !a = S.unsafeTake i1 s
        when (i2 < S.length s) $ do
            let !b = S.unsafeDrop i2 s
            Streams.unRead b input

        -- Optimize for the common case: dl is almost always "id"
        let !out = if null l then a else S.concat (reverse (a:l))
        return out

    noCRLF l s = go (s:l)

    lastIsCR l s idx# = do
        !t <- Streams.read input >>= maybe throwNoCRLF return
        if S.null t
          then lastIsCR l s idx#
          else do
            let !c = S.unsafeHead t
            if c /= 10
              then throwBadCRLF
              else do
                  let !a = S.unsafeTake (I# idx#) s
                  let !b = S.unsafeDrop 1 t
                  when (not $ S.null b) $ Streams.unRead b input
                  let !out = if null l then a else S.concat (reverse (a:l))
                  return out


------------------------------------------------------------------------------
data CS = FoundCRLF !Int#
        | NoCR
        | LastIsCR !Int#
        | BadCR


------------------------------------------------------------------------------
findCRLF :: ByteString -> CS
findCRLF b =
    case S.elemIndex '\r' b of
      Nothing         -> NoCR
      Just !i@(I# i#) ->
          let !i' = i + 1
          in if i' < S.length b
               then if S.unsafeIndex b i' == 10
                      then FoundCRLF i#
                      else BadCR
               else LastIsCR i#
{-# INLINE findCRLF #-}


------------------------------------------------------------------------------
splitCh :: Char -> ByteString -> (ByteString, ByteString)
splitCh !c !s = maybe (s, S.empty) f (S.elemIndex c s)
  where
    f !i = let !a = S.unsafeTake i s
               !b = S.unsafeDrop (i + 1) s
           in (a, b)
{-# INLINE splitCh #-}


------------------------------------------------------------------------------
breakCh :: Char -> ByteString -> (ByteString, ByteString)
breakCh !c !s = maybe (s, S.empty) f (S.elemIndex c s)
  where
    f !i = let !a = S.unsafeTake i s
               !b = S.unsafeDrop i s
           in (a, b)
{-# INLINE breakCh #-}


------------------------------------------------------------------------------
splitHeader :: ByteString -> (ByteString, ByteString)
splitHeader !s = maybe (s, S.empty) f (S.elemIndex ':' s)
  where
    l = S.length s

    f i = let !a = S.unsafeTake i s
          in (a, skipSp (i + 1))

    skipSp !i | i >= l    = S.empty
              | otherwise = let c = S.unsafeIndex s i
                            in if isLWS $ w2c c
                                 then skipSp $ i + 1
                                 else S.unsafeDrop i s

{-# INLINE splitHeader #-}



------------------------------------------------------------------------------
isLWS :: Char -> Bool
isLWS c = c == ' ' || c == '\t'
{-# INLINE isLWS #-}


------------------------------------------------------------------------------

{-
    Read the remainder of the response message's header section,
    parsing into key/value pairs. Note that this function terminates
    when it hits the "blank" line (ie, CRLF CRLF pair), which it
    consumes.
-}
readHeaderFields :: InputStream ByteString -> IO [(ByteString,ByteString)]
readHeaderFields input = do
    f <- go id
    return $! f []

  where
    go !dlistSoFar = do
        line <- readResponseLine input
        if S.null line
          then return dlistSoFar
          else do
            let (!k,!v) = splitHeader line
            vf <- pCont id
            let vs = vf []
            let !v' = if null vs then v else S.concat (v:vs)
            let !t = (k,v')
            go (dlistSoFar . (t:))

      where
        trimBegin = S.dropWhile isLWS

        pCont !dlist = do
            mbS  <- Streams.peek input
            maybe (return dlist)
                  (\s -> if S.null s
                           then Streams.read input >> pCont dlist
                           else if isLWS $ w2c $ S.unsafeHead s
                                  then procCont dlist
                                  else return dlist)
                  mbS

        procCont !dlist = do
            line <- readResponseLine input
            let !t = trimBegin line
            pCont (dlist . (" ":) . (t:))



                            -----------------------
                            -- utility functions --
                            -----------------------


------------------------------------------------------------------------------
-- | Note: only works for nonnegative naturals
unsafeFromNat :: (Enum a, Num a, Bits a) => ByteString -> a
unsafeFromNat = S.foldl' f 0
  where
    zero = ord '0'
    f !cnt !i = cnt * 10 + toEnum (digitToInt i)

    digitToInt c = if d >= 0 && d <= 9
                     then d
                     else error $ "bad digit: '" ++ [c] ++ "'"
      where
        !d = ord c - zero
{-# INLINE unsafeFromNat #-}
