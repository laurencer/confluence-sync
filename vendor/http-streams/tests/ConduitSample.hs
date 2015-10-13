--
-- Benchmark code: sample request using http-condiuit
--
-- Copyright © 2012-2014 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is made
-- available to you by its authors as open source software: you can
-- redistribute it and/or modify it under a BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module ConduitSample (sampleViaHttpConduit) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.CaseInsensitive (CI, original)
import Data.Conduit
import Data.Conduit.Binary (sinkHandle, sourceLbs)
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.IO (IOMode (..), hClose, openFile)

main :: IO ()
main = do
    withManager $ liftIO . sampleViaHttpConduit

sampleViaHttpConduit :: Manager -> IO ()
sampleViaHttpConduit manager = do

    runResourceT $ do

        req <- parseUrl "http://localhost/"
        let req2 = req {
            checkStatus = \_ _ _ -> Nothing,
            requestHeaders = [(hAccept, "text/html")],
            responseTimeout = Nothing
        }

        res <- http req2 manager

        let sta = responseStatus res
            ver = responseVersion res
            hdr = responseHeaders res

        handle <- liftIO $ openFile "/tmp/http-conduit.out" WriteMode

        let src = do
                sourceLbs (joinStatus sta ver)
                sourceLbs (join hdr)
        src $$ sinkHandle handle
        responseBody res $$+- sinkHandle handle
        liftIO $ hClose handle


joinStatus :: Status -> HttpVersion -> L.ByteString
joinStatus sta ver =
    L.concat $ map L.pack
        [ show ver, " "
        , show $ statusCode sta, " "
        , S.unpack $ statusMessage sta
        , "\n"
        ]

--
-- Process headers into a single string
--

join :: ResponseHeaders -> L.ByteString
join m =
    foldr combineHeaders "" m


combineHeaders :: (CI S.ByteString, S.ByteString) -> L.ByteString -> L.ByteString
combineHeaders (k,v) acc =
    L.append acc $ L.fromChunks [key, ": ", value, "\n"]
  where
    key = original k
    value = v
