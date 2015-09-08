-- This module primarily exists to separate the Crypto.Hash
-- dependency from the rest of the codebase. At the moment -
-- it doesn't play nicely with doctest.
module Confluence.Sync.Internal.AttachmentHash (
  attachmentHash
  ) where

import           Crypto.Hash

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BSC
import           Data.Tree.Zipper

import           Confluence.Sync.LocalSite

attachmentHash :: SiteFileZipper -> IO String
attachmentHash zipper = do
  contents <- LBS.readFile . filePath . label $ zipper
  return . BSC.unpack $ digestToHexByteString $ md5 contents
  where md5 :: LBS.ByteString -> Digest MD5
        md5 = hashlazy
