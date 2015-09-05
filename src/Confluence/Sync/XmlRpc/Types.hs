{-# LANGUAGE RecordWildCards #-}

module Confluence.Sync.XmlRpc.Types (
  NewPage(..)
, Page(..)
  
, PageSummary(..)
, pageSummaryFromPage

, Attachment(..)
, NewAttachment(..)
) where

import Prelude hiding (id)

import Data.Maybe
import Data.Int

import Data.Time.LocalTime

import Network.XmlRpc.Internals

data NewPage = NewPage {
  newPageSpace :: String
, newPageParentId :: Maybe String
, newPageTitle :: String
, newPageContent :: String
, newPagePermissions :: Maybe String
} deriving Show

instance XmlRpcType NewPage where
  toValue p = toValue $ catMaybes $ map liftMaybe [
        ("space", Just $ toValue (newPageSpace p))
      , ("parentId", fmap toValue (newPageParentId p))
      , ("title", Just $ toValue (newPageTitle p))
      , ("content", Just $ toValue (newPageContent p))
      , ("permissions", fmap toValue (newPagePermissions p))
    ]

  fromValue v = do
    t <- fromValue v
    newPageSpace     <- getField "space" t
    newPageParentId  <- getFieldMaybe "parentId" t
    newPageTitle     <- getField "title" t
    newPageContent   <- getField "content" t
    newPagePermissions   <- getFieldMaybe "permissions" t
    return NewPage { .. }

  getType _ = TStruct

data Page = Page { 
  pageId :: String
, pageSpace :: String
, pageParentId :: String
, pageTitle :: String
, pageUrl :: String
, pageVersion :: Maybe String
, pageContent :: String
, pageCreated :: Maybe LocalTime
, pageCreator :: Maybe String
, pageModified :: Maybe LocalTime
, pageModifier :: Maybe String
, pageHomePage :: Maybe String
, pagePermissions :: String
, pageContentStatus :: Maybe String
, pageCurrent :: Maybe String
  } deriving (Eq, Show)

instance XmlRpcType Page where
  toValue p = toValue $ catMaybes $ map liftMaybe [
        ("id", Just $ toValue (pageId p))
      , ("space", Just $ toValue (pageSpace p))
      , ("parentId", Just $ toValue (pageParentId p))
      , ("title", Just $ toValue (pageTitle p))
      , ("url", Just $ toValue (pageUrl p))
      , ("version", fmap toValue (pageVersion p))
      , ("content", Just $ toValue (pageContent p))
      , ("created", fmap toValue (pageCreated p))
      , ("creator", fmap toValue (pageCreator p))
      , ("modified", fmap toValue (pageModified p))
      , ("modifier", fmap toValue (pageModifier p))
      , ("current", fmap toValue (pageCurrent p))
      , ("homePage", fmap toValue (pageHomePage p))
      , ("permissions", Just $ toValue (pagePermissions p))
      , ("contentStatus", fmap toValue (pageContentStatus p))
    ]

  fromValue v = do
    t <- fromValue v
    pageId        <- getField "id" t
    pageSpace     <- getField "space" t
    pageParentId  <- getField "parentId" t
    pageTitle     <- getField "title" t
    pageUrl       <- getField "url" t
    pageVersion   <- getFieldMaybe "version" t
    pageContent   <- getField "content" t
    pageCreated   <- getFieldMaybe "created" t
    pageCreator   <- getFieldMaybe "creator" t
    pageModified  <- getFieldMaybe "modified" t
    pageModifier  <- getFieldMaybe "modifier" t
    pageHomePage  <- getFieldMaybe "homePage" t
    pageCurrent   <- getFieldMaybe "current" t
    pagePermissions   <- getField "permissions" t
    pageContentStatus <- getFieldMaybe "contentStatus" t
    return Page { .. }

  getType _ = TStruct

data PageSummary = PageSummary { 
  pageSummaryId :: String
, pageSummarySpace :: String
, pageSummaryParentId :: String
, pageSummaryTitle :: String
, pageSummaryUrl :: String
, pageSummaryPermissions :: String
  } deriving Show

pageSummaryFromPage :: Page -> PageSummary
pageSummaryFromPage p = PageSummary { .. }
  where pageSummaryId           = pageId p
        pageSummarySpace        = pageSpace p
        pageSummaryParentId     = pageParentId p
        pageSummaryTitle        = pageTitle p
        pageSummaryUrl          = pageUrl p
        pageSummaryPermissions  = pagePermissions p


instance XmlRpcType PageSummary where
  toValue p = toValue $ [
        ("id", toValue (pageSummaryId p))
      , ("space", toValue (pageSummarySpace p))
      , ("parentId", toValue (pageSummaryParentId p))
      , ("title", toValue (pageSummaryTitle p))
      , ("url", toValue (pageSummaryUrl p))
      , ("permissions", toValue (pageSummaryPermissions p))
    ]

  fromValue v = do
    t <- fromValue v
    pageSummaryId        <- getField "id" t
    pageSummarySpace     <- getField "space" t
    pageSummaryParentId  <- getField "parentId" t
    pageSummaryTitle     <- getField "title" t
    pageSummaryUrl       <- getField "url" t
    pageSummaryPermissions   <- getField "permissions" t
    return PageSummary { .. }

  getType _ = TStruct

data Attachment = Attachment {
  attachmentId :: String
, attachmentPageId :: String
, attachmentTitle :: String
, attachmentFileName :: String
, attachmentFileSize :: String
, attachmentContentType :: String
, attachmentCreated :: LocalTime
, attachmentCreator :: String
, attachmentUrl :: String
, attachmentComment :: Maybe String
} deriving Show

instance XmlRpcType Attachment where
  toValue p = toValue $ catMaybes $ map liftMaybe [
        ("id", Just $ toValue (attachmentId p))
      , ("pageId", Just $ toValue (attachmentPageId p))
      , ("title", Just $ toValue (attachmentTitle p))
      , ("fileName", Just $ toValue (attachmentFileName p))
      , ("fileSize", Just $ toValue (attachmentFileSize p))
      , ("contentType", Just $ toValue (attachmentContentType p))
      , ("created", Just $ toValue (attachmentCreated p))
      , ("creator", Just $ toValue (attachmentCreator p))
      , ("url", Just $ toValue (attachmentUrl p))
      , ("comment", fmap toValue (attachmentComment p))
    ]

  fromValue v = do
    t <- fromValue v
    attachmentId           <- getField "id" t
    attachmentPageId       <- getField "pageId" t
    attachmentTitle        <- getField "title" t
    attachmentFileName     <- getField "fileName" t
    attachmentFileSize     <- getField "fileSize" t
    attachmentContentType  <- getField "contentType" t
    attachmentCreated      <- getField "created" t
    attachmentCreator      <- getField "creator" t
    attachmentUrl          <- getField "url" t
    attachmentComment      <- getFieldMaybe "comment" t
    return Attachment { .. }

  getType _ = TStruct

data NewAttachment = NewAttachment {
  newAttachmentFileName     :: String
, newAttachmentContentType  :: String
} deriving Show

instance XmlRpcType NewAttachment where
  toValue p = toValue $ catMaybes $ map liftMaybe [
      ("fileName", Just $ toValue (newAttachmentFileName p))
    , ("contentType", Just $ toValue (newAttachmentContentType p))
    ]

  fromValue v = do
    t <- fromValue v
    newAttachmentFileName     <- getField "fileName" t
    newAttachmentContentType  <- getField "contentType" t
    return NewAttachment { .. }

  getType _ = TStruct


-- Helper Functions

liftMaybe :: (a, Maybe b) -> Maybe ((a, b))
liftMaybe (a, Just b)  = Just (a, b)
liftMaybe (a, Nothing) = Nothing
