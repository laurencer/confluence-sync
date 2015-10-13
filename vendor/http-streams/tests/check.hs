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
{-# OPTIONS -fno-warn-unused-imports #-}

module Main where

import OpenSSL (withOpenSSL)
import Test.Hspec (hspec)

import MockServer (runMockServer)
import TestSuite (suite)

main :: IO ()
main = withOpenSSL $ do
    runMockServer
    hspec suite
