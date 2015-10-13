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

module Snippet where

import Control.Applicative
import Control.Exception (bracket)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types (parseEither)
import Data.Attoparsec.ByteString
import Data.Text hiding (empty)
import GHC.Generics
import Network.Http.Client
import qualified System.IO.Streams.Attoparsec as Streams

--
-- Otherwise redundent imports, but useful for testing in GHCi.
--

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Debug.Trace
import System.Exit (exitSuccess)
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams

main = main0

blob = "{\"name\": \"Kennedy\"}" :: L.ByteString


main0 :: IO ()
main0 = do
    c <- openConnection "ip.jsontest.com" 80

    let q = buildRequest1 $ do
                http GET "/"
                setAccept "application/json"

    putStr $ show q
            -- Requests [headers] are terminated by a double newline
            -- already. We need a better way of emitting debug
            -- information mid-stream from this library.

    sendRequest c q emptyBody

    x <- receiveResponse c jsonHandler :: IO IP
    L.putStr $ encodePretty x

    closeConnection c


experiment :: IO ()
experiment = do
    let (Just result) = decode blob :: Maybe Person
    putStr $ show result
{-
    let ve = parseOnly json' blob
    case ve of
        Left str    ->  error str
        Right v     ->  L.putStr $ encodePretty v
-}


data Person = Person {
    name :: Text
} deriving (Show)


instance FromJSON Person where
    parseJSON (Object o) = do
                               n <- o .: "name"
                               return Person { name = n }
    parseJSON _          = empty



data IP = IP {
    ip :: Text
} deriving (Show, Generic)

instance FromJSON IP
instance ToJSON IP
