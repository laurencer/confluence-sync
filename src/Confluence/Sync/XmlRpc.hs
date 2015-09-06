{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Confluence.Sync.XmlRpc (
  Token
, ApiCall
, runApiCall
, login
, logout

, getPage
, getPageByName
, getDescendents

, storePage
, createPage

, getAttachments
, addAttachment
, removeAttachment
) where

import Data.ByteString
import Data.Int
import Data.Time.LocalTime

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader

import Network.XmlRpc.Client
import Network.XmlRpc.Internals

import Confluence.Sync.XmlRpc.Types
import Confluence.Sync.RateLimiter

type ConfluenceUrl      = String
type Username           = String
type Password           = String
type Token              = String
type PageId             = String
type PageName           = String
type SpaceKey           = String
type AttachmentFileName = String
type ApiContext         = (Throttle, ConfluenceUrl, Token)
type RequestMethod      = String
type ErrorMessage       = String
type ApiCall a          = ReaderT ApiContext (ExceptT ErrorMessage IO) a

login :: ConfluenceUrl -> Username -> Password -> IO Token
login url = remote url "confluence2.login"

-- 
runApiCall :: Throttle -> ConfluenceUrl -> Token -> ApiCall a -> IO (Either ErrorMessage a)
runApiCall throttle url token apiCall = runExceptT $ (flip runReaderT) (throttle, url, token) apiCall

-- | Creates a function that automatically marshal
--   arguments and the return type.
invoke :: Callable a => RequestMethod -> a
invoke = _invoke throttledCall
  where rawCall :: ConfluenceUrl -> RequestMethod -> Token -> [Value] -> Err IO Value
        rawCall url method token arguments = call url method ((toValue token) : arguments)
        -- | This ensures that errors are wrapped and kept in the ExceptT instead of thrown
        --   when the final IO action is run.
        errorHandlingCall :: ConfluenceUrl -> RequestMethod -> Token -> [Value] -> Err IO Value
        errorHandlingCall url method token arguments = 
          (fmap (ExceptT . return) invoked) >>= id
          where invoked = liftError (runExceptT $ rawCall url method token arguments)
                liftError :: IO a -> ExceptT ErrorMessage IO a
                liftError action = ExceptT $ catch (fmap Right action) (\(ex :: IOException) -> return $ Left (show ex))
        -- | This ensures that actions are throttled at the correct rate.
        --   This needs to be applied after the error handling because the Throttle
        --   uses an IO error to communicate when the limit has been exceeded.
        -- TODO: change the throttle error to an explicit return value.
        throttledCall :: Throttle -> ConfluenceUrl -> RequestMethod -> Token -> [Value] -> Err IO Value
        throttledCall throttle url method token arguments = 
          ExceptT $ runThrottledAction throttle $ runExceptT $ errorHandlingCall url method token arguments


class Callable a where
  _invoke :: (Throttle -> ConfluenceUrl -> RequestMethod -> Token -> [Value] -> Err IO Value)
          -> (RequestMethod -> a)

instance XmlRpcType a => Callable (ApiCall a) where
  _invoke f m = do
    (throttle, url, token) <- ask
    lift $ f throttle url m token [] >>= fromValue

-- Creates an instance for any function args.
instance (XmlRpcType a, Callable b) => Callable (a -> b) where
  -- e: error handling function
  -- f: original function that will be wrapped (to inject the current function argument)
  -- m: original method name
  -- x: new argument injected by the current function.
  _invoke f m x = _invoke (\thr u m t xs -> f thr u m t (toValue x:xs)) m

logout :: ApiCall Bool
logout = invoke "confluence2.logout"

getPage :: PageId -> ApiCall Page
getPage = invoke "confluence2.getPage"

getPageByName :: SpaceKey -> PageName -> ApiCall Page
getPageByName = invoke "confluence2.getPage"

getDescendents :: PageId -> ApiCall [PageSummary]
getDescendents = invoke "confluence2.getDescendents"

getAttachments :: PageId -> ApiCall [Attachment]
getAttachments = invoke "confluence2.getAttachments"

createPage :: NewPage -> ApiCall Page
createPage = invoke "confluence2.storePage"

storePage :: Page -> ApiCall Page
storePage = invoke "confluence2.storePage"

movePage :: PageId -> PageId -> String -> ApiCall String
movePage = invoke "confluence2.movePage"

addAttachment :: PageId -> NewAttachment -> ByteString -> ApiCall Attachment
addAttachment = invoke "confluence2.addAttachment"

removeAttachment :: PageId -> AttachmentFileName -> ApiCall Bool
removeAttachment = invoke "confluence2.removeAttachment"
