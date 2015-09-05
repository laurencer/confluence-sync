{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XmlRpc.Internals
-- Copyright   :  (c) Bjorn Bringert 2003
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (requires extensions and non-portable libraries)
--
-- This module contains the core functionality of the XML-RPC library.
-- Most applications should not need to use this module. Client
-- applications should use "Network.XmlRpc.Client" and server applications should
-- use "Network.XmlRpc.Server".
--
-- The XML-RPC specifcation is available at <http://www.xmlrpc.com/spec>.
--
-----------------------------------------------------------------------------

module Network.XmlRpc.Internals (
-- * Method calls and repsonses
MethodCall(..), MethodResponse(..),
-- * XML-RPC types
Value(..), Type(..), XmlRpcType(..),
-- * Converting from XML
parseResponse, parseCall, getField, getFieldMaybe,
-- * Converting to XML
renderCall, renderResponse,
-- * Converting to and from DTD types
toXRValue, fromXRValue,
toXRMethodCall, fromXRMethodCall,
toXRMethodResponse, fromXRMethodResponse,
toXRParams, fromXRParams,
toXRMember, fromXRMember,
-- * Error monad
Err, maybeToM, handleError, ioErrorToErr
) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.Format
import           Data.Time.LocalTime
import           Numeric (showFFloat)
import           Prelude hiding (showString, catch)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Time (CalendarTime(..))

#if ! MIN_VERSION_time(1,5,0)
import           System.Locale (defaultTimeLocale)
#endif

import qualified Data.ByteString.Char8 as BS (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString, pack)
import qualified Network.XmlRpc.Base64 as Base64
import qualified Network.XmlRpc.DTD_XMLRPC as XR
import           Network.XmlRpc.Pretty
import           Text.XML.HaXml.XmlContent

--
-- General utilities
--

-- | Replaces all occurances of a sublist in a list with another list.
--   If the list to replace is the empty list, does nothing.
replace :: Eq a =>
	[a] -- ^ The sublist to replace when found
	-> [a] -- ^ The list to replace it with
	-> [a] -- ^ The list to replace in
	-> [a] -- ^ The result
replace [] _ xs = xs
replace _ _ [] = []
replace ys zs xs@(x:xs')
    | isPrefixOf ys xs = zs ++ replace ys zs (drop (length ys) xs)
    | otherwise = x : replace ys zs xs'

-- | Convert a 'Maybe' value to a value in any monad
maybeToM :: Monad m =>
		String -- ^ Error message to fail with for 'Nothing'
	     -> Maybe a -- ^ The 'Maybe' value.
	     -> m a -- ^ The resulting value in the monad.
maybeToM err Nothing = fail err
maybeToM _ (Just x) = return x

-- | Convert a 'Maybe' value to a value in any monad
eitherToM :: Monad m
          => String -- ^ Error message to fail with for 'Nothing'
	  -> Either String a -- ^ The 'Maybe' value.
	  -> m a -- ^ The resulting value in the monad.
eitherToM err (Left s)  = fail (err ++ ": " ++ s)
eitherToM   _ (Right x) = return x

-- | The format for \"dateTime.iso8601\"
xmlRpcDateFormat :: String
xmlRpcDateFormat = "%Y%m%dT%H:%M:%S"

--
-- Error monad stuff
--

type Err m a = ExceptT String m a

-- | Evaluate the argument and catch error call exceptions
errorToErr :: (Show e, MonadError e m) => a -> Err m a
errorToErr x = unsafePerformIO (liftM return (evaluate x) `catch` handleErr)
  where handleErr :: Monad m => SomeException -> IO (Err m a)
        handleErr = return . throwError . show

-- | Catch IO errors in the error monad.
ioErrorToErr :: IO a -> Err IO a
ioErrorToErr x = (liftIO x >>= return) `catchError` \e -> throwError (show e)

-- | Handle errors from the error monad.
handleError :: Monad m => (String -> m a) -> Err m a -> m a
handleError h m = do
		  Right x <- runExceptT (catchError m (lift . h))
		  return x

errorRead :: (Monad m, Read a) =>
	     ReadS a -- ^ Parser
	  -> String -- ^ Error message
	  -> String -- ^ String to parse
	  -> Err m a
errorRead r err s = case [x | (x,t) <- r s, ("","") <- lex t] of
		          [x] -> return x
		          _   -> fail (err ++ ": '" ++ s ++ "'")

--
-- Types for methods calls and responses
--

-- | An XML-RPC method call. Consists of a method name and a list of
--   parameters.
data MethodCall = MethodCall String [Value]
		  deriving (Eq, Show) -- for debugging

-- | An XML-RPC response.
data MethodResponse = Return Value -- ^ A method response returning a value
		    | Fault Int String -- ^ A fault response
		      deriving (Eq, Show) -- for debugging

-- | An XML-RPC value.
data Value =
      ValueInt Int -- ^ int, i4, or i8
    | ValueBool Bool -- ^ bool
    | ValueString String -- ^ string
    | ValueUnwrapped String -- ^ no inner element
    | ValueDouble Double -- ^ double
    | ValueDateTime LocalTime -- ^ dateTime.iso8601
    | ValueBase64 BS.ByteString -- ^ base 64.  NOTE that you should provide the raw data; the haxr library takes care of doing the base-64 encoding.
    | ValueStruct [(String,Value)] -- ^ struct
    | ValueArray [Value]  -- ^ array
      deriving (Eq, Show) -- for debugging

-- | An XML-RPC value. Use for error messages and introspection.
data Type =
	  TInt
	  | TBool
	  | TString
	  | TDouble
	  | TDateTime
	  | TBase64
	  | TStruct
	  | TArray
	  | TUnknown
      deriving (Eq)

instance Show Type where
    show TInt = "int"
    show TBool = "bool"
    show TString = "string"
    show TDouble = "double"
    show TDateTime = "dateTime.iso8601"
    show TBase64 = "base64"
    show TStruct = "struct"
    show TArray = "array"
    show TUnknown = "unknown"

instance Read Type where
    readsPrec _ s = case break isSpace (dropWhile isSpace s) of
		    ("int",r) -> [(TInt,r)]
		    ("bool",r) -> [(TBool,r)]
		    ("string",r) -> [(TString,r)]
		    ("double",r) -> [(TDouble,r)]
		    ("dateTime.iso8601",r) -> [(TDateTime,r)]
		    ("base64",r) -> [(TBase64,r)]
		    ("struct",r) -> [(TStruct,r)]
		    ("array",r) -> [(TArray,r)]

-- | Gets the value of a struct member
structGetValue :: Monad m => String -> Value -> Err m Value
structGetValue n (ValueStruct t) =
    maybeToM ("Unknown member '" ++ n ++ "'") (lookup n t)
structGetValue _ _ = fail "Value is not a struct"

-- | Builds a fault struct
faultStruct :: Int -> String -> Value
faultStruct code str = ValueStruct [("faultCode",ValueInt code),
				    ("faultString",ValueString str)]

-- XML-RPC specification:
-- "The body of the response is a single XML structure, a
-- <methodResponse>, which can contain a single <params> which contains a
-- single <param> which contains a single <value>."
onlyOneResult :: Monad m => [Value] -> Err m Value
onlyOneResult [] = fail "Method returned no result"
onlyOneResult [x] = return x
onlyOneResult _ = fail "Method returned more than one result"

--
-- Converting to and from XML-RPC types
--

-- | A class for mapping Haskell types to XML-RPC types.
class XmlRpcType a where
    -- | Convert from this type to a 'Value'
    toValue :: a -> Value
    -- | Convert from a 'Value' to this type. May fail if
    --   if there is a type error.
    fromValue :: Monad m => Value -> Err m a
    getType :: a -> Type

typeError :: (XmlRpcType a, Monad m) => Value -> Err m a
typeError v = withType $ \t ->
       fail ("Wanted: "
	     ++ show (getType t)
	     ++ "', got: '"
	     ++ showXml False (toXRValue v) ++ "'") `asTypeOf` return t

-- a type hack for use in 'typeError'
withType :: (a -> Err m a) -> Err m a
withType f = f undefined

simpleFromValue :: (Monad m, XmlRpcType a) => (Value -> Maybe a)
		-> Value -> Err m a
simpleFromValue f v =
    maybe (typeError v) return (f v)


-- | Exists to allow explicit type conversions.
instance XmlRpcType Value where
    toValue = id
    fromValue = return . id
    getType _ = TUnknown

-- FIXME: instance for ()?


instance XmlRpcType Int where
    toValue = ValueInt
    fromValue = simpleFromValue f
	where f (ValueInt x) = Just x
	      f _ = Nothing
    getType _ = TInt

instance XmlRpcType Bool where
    toValue = ValueBool
    fromValue = simpleFromValue f
	where f (ValueBool x) = Just x
	      f _ = Nothing
    getType _ = TBool

instance XmlRpcType String where
    toValue = ValueString
    fromValue = simpleFromValue f
	where f (ValueString x) = Just x
              f (ValueUnwrapped x) = Just x
	      f _ = Nothing
    getType _ = TString

instance XmlRpcType BS.ByteString where
    toValue = ValueBase64
    fromValue = simpleFromValue f
        where f (ValueBase64 x) = Just x
              f _ = Nothing
    getType _ = TBase64

instance XmlRpcType Double where
    toValue = ValueDouble
    fromValue = simpleFromValue f
	where f (ValueDouble x) = Just x
	      f _ = Nothing
    getType _ = TDouble

instance XmlRpcType LocalTime where
    toValue = ValueDateTime
    fromValue = simpleFromValue f
	where f (ValueDateTime x) = Just x
	      f _ = Nothing
    getType _ = TDateTime

instance XmlRpcType CalendarTime where
    toValue = toValue . calendarTimeToLocalTime
    fromValue = liftM localTimeToCalendarTime . fromValue
    getType _ = TDateTime

-- FIXME: array elements may have different types
instance XmlRpcType a => XmlRpcType [a] where
    toValue = ValueArray . map toValue
    fromValue v = case v of
			 ValueArray xs -> mapM fromValue xs
			 _ -> typeError v
    getType _ = TArray

-- FIXME: struct elements may have different types
instance XmlRpcType a => XmlRpcType [(String,a)] where
    toValue xs = ValueStruct [(n, toValue v) | (n,v) <- xs]

    fromValue v = case v of
		  ValueStruct xs -> mapM (\ (n,v') -> liftM ((,) n) (fromValue v')) xs
		  _ -> typeError v
    getType _ = TStruct

-- Tuple instances may be used for heterogenous array types.
instance (XmlRpcType a, XmlRpcType b, XmlRpcType c, XmlRpcType d,
          XmlRpcType e) =>
         XmlRpcType (a,b,c,d,e) where
    toValue (v,w,x,y,z) =
        ValueArray [toValue v, toValue w, toValue x, toValue y, toValue z]
    fromValue (ValueArray [v,w,x,y,z]) =
        liftM5 (,,,,) (fromValue v) (fromValue w) (fromValue x)
                      (fromValue y) (fromValue z)
    fromValue _ = throwError "Expected 5-element tuple!"
    getType _ = TArray

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c, XmlRpcType d) =>
         XmlRpcType (a,b,c,d) where
    toValue (w,x,y,z) = ValueArray [toValue w, toValue x, toValue y, toValue z]
    fromValue (ValueArray [w,x,y,z]) =
        liftM4 (,,,) (fromValue w) (fromValue x) (fromValue y) (fromValue z)
    fromValue _ = throwError "Expected 4-element tuple!"
    getType _ = TArray

instance (XmlRpcType a, XmlRpcType b, XmlRpcType c) => XmlRpcType (a,b,c) where
    toValue (x,y,z) = ValueArray [toValue x, toValue y, toValue z]
    fromValue (ValueArray [x,y,z]) =
        liftM3 (,,) (fromValue x) (fromValue y) (fromValue z)
    fromValue _ = throwError "Expected 3-element tuple!"
    getType _ = TArray

instance (XmlRpcType a, XmlRpcType b) => XmlRpcType (a,b) where
    toValue (x,y) = ValueArray [toValue x, toValue y]
    fromValue (ValueArray [x,y]) = liftM2 (,) (fromValue x) (fromValue y)
    fromValue _ = throwError "Expected 2-element tuple."
    getType _ = TArray

-- | Get a field value from a (possibly heterogeneous) struct.
getField :: (Monad m, XmlRpcType a) =>
	    String           -- ^ Field name
	 -> [(String,Value)] -- ^ Struct
	 -> Err m a
getField x xs = maybeToM ("struct member " ++ show x ++ " not found")
		(lookup x xs) >>= fromValue

-- | Get a field value from a (possibly heterogeneous) struct.
getFieldMaybe :: (Monad m, XmlRpcType a) =>
	    String           -- ^ Field name
	 -> [(String,Value)] -- ^ Struct
	 -> Err m (Maybe a)
getFieldMaybe x xs = case lookup x xs of
				      Nothing -> return Nothing
				      Just v -> liftM Just (fromValue v)

--
-- Converting to XR types
--

toXRValue :: Value -> XR.Value
toXRValue (ValueInt x) = XR.Value [XR.Value_AInt (XR.AInt (showInt x))]
toXRValue (ValueBool b) = XR.Value [XR.Value_Boolean (XR.Boolean (showBool b))]
toXRValue (ValueString s) = XR.Value [XR.Value_AString (XR.AString (showString s))]
toXRValue (ValueUnwrapped s) = XR.Value [XR.Value_Str s]
toXRValue (ValueDouble d) = XR.Value [XR.Value_ADouble (XR.ADouble (showDouble d))]
toXRValue (ValueDateTime t) =
   XR.Value [ XR.Value_DateTime_iso8601 (XR.DateTime_iso8601 (showDateTime t))]
toXRValue (ValueBase64 s) = XR.Value [XR.Value_Base64 (XR.Base64 (showBase64 s))]
toXRValue (ValueStruct xs) = XR.Value [XR.Value_Struct (XR.Struct (map toXRMember xs))]
toXRValue (ValueArray xs) =
    XR.Value [XR.Value_Array (XR.Array (XR.Data (map toXRValue xs)))]

showInt :: Int -> String
showInt = show

showBool :: Bool -> String
showBool b = if b then "1" else "0"

-- escapes & and <
showString :: String -> String
showString s = replace ">" "&gt;" $ replace "<" "&lt;" (replace "&" "&amp;" s)

-- | Shows a double in signed decimal point notation.
showDouble :: Double -> String
showDouble d = showFFloat Nothing d ""

-- | Shows a date and time on the format: YYYYMMDDTHH:mm:SS
showDateTime :: LocalTime -> String
showDateTime t = formatTime defaultTimeLocale xmlRpcDateFormat t

showBase64 :: BS.ByteString -> String
showBase64 = BS.unpack . Base64.encode

toXRMethodCall :: MethodCall -> XR.MethodCall
toXRMethodCall (MethodCall name vs) =
    XR.MethodCall (XR.MethodName name) (Just (toXRParams vs))

toXRMethodResponse :: MethodResponse -> XR.MethodResponse
toXRMethodResponse (Return val) = XR.MethodResponseParams (toXRParams [val])
toXRMethodResponse (Fault code str) =
    XR.MethodResponseFault (XR.Fault (toXRValue (faultStruct code str)))

toXRParams :: [Value] -> XR.Params
toXRParams vs = XR.Params (map (XR.Param . toXRValue) vs)

toXRMember :: (String, Value) -> XR.Member
toXRMember (n, v) = XR.Member (XR.Name n) (toXRValue v)

--
-- Converting from XR types
--

fromXRValue :: Monad m => XR.Value -> Err m Value
fromXRValue (XR.Value vs)
  =  case (filter notstr vs) of
       []     -> liftM  (ValueUnwrapped . concat) (mapM (readString . unstr) vs)
       (v:_)  -> f v
  where
  notstr (XR.Value_Str _)  = False
  notstr _                 = True

  unstr (XR.Value_Str x)  = x

  f (XR.Value_I4 (XR.I4 x)) = liftM ValueInt (readInt x)
  f (XR.Value_I8 (XR.I8 x)) = liftM ValueInt (readInt x)
  f (XR.Value_AInt (XR.AInt x)) = liftM ValueInt (readInt x)
  f (XR.Value_Boolean (XR.Boolean x)) = liftM ValueBool (readBool x)
  f (XR.Value_ADouble (XR.ADouble x)) = liftM ValueDouble (readDouble x)
  f (XR.Value_AString (XR.AString x)) = liftM ValueString (readString x)
  f (XR.Value_DateTime_iso8601 (XR.DateTime_iso8601 x)) =
    liftM ValueDateTime (readDateTime x)
  f (XR.Value_Base64 (XR.Base64 x)) = liftM ValueBase64 (readBase64 x)
  f (XR.Value_Struct (XR.Struct ms)) =
    liftM ValueStruct (mapM fromXRMember ms)
  f (XR.Value_Array (XR.Array (XR.Data xs))) =
    liftM ValueArray (mapM fromXRValue xs)


fromXRMember :: Monad m => XR.Member -> Err m (String,Value)
fromXRMember (XR.Member (XR.Name n) xv) = liftM (\v -> (n,v)) (fromXRValue xv)

-- | From the XML-RPC specification:
--
-- \"An integer is a 32-bit signed number. You can include a plus or
-- minus at the beginning of a string of numeric characters. Leading
-- zeros are collapsed. Whitespace is not permitted. Just numeric
-- characters preceeded by a plus or minus.\"
readInt :: Monad m => String -> Err m Int
readInt s = errorRead reads "Error parsing integer" s


-- | From the XML-RPC specification:
--
-- \"0 (false) or 1 (true)\"
readBool :: Monad m => String -> Err m Bool
readBool s = errorRead readsBool "Error parsing boolean" s
    where readsBool "true" = [(True,"")]
	  readsBool "false" = [(False,"")]
	  readsBool "1" = [(True,"")]
	  readsBool "0" = [(False,"")]
	  readsBool _ = []

-- | From the XML-RPC specification:
--
-- \"Any characters are allowed in a string except \< and &, which are
-- encoded as &lt; and &amp;. A string can be used to encode binary data.\"
--
-- To work with implementations (such as some Python bindings for example)
-- which also escape \>, &gt; is unescaped. This is non-standard, but
-- seems unlikely to cause problems.
readString :: Monad m => String -> Err m String
readString = return . replace "&amp;" "&" . replace "&lt;" "<"
	     . replace "&gt;" ">"


-- | From the XML-RPC specification:
--
-- There is no representation for infinity or negative infinity or \"not
-- a number\". At this time, only decimal point notation is allowed, a
-- plus or a minus, followed by any number of numeric characters,
-- followed by a period and any number of numeric
-- characters. Whitespace is not allowed. The range of allowable values
-- is implementation-dependent, is not specified.
--
-- FIXME: accepts more than decimal point notation
readDouble :: Monad m => String -> Err m Double
readDouble s = errorRead reads "Error parsing double" s

-- | From <http://groups.yahoo.com/group/xml-rpc/message/4733>:
--
--   \"Essentially \"dateTime.iso8601\" is a misnomer and the format of the
--   content of this element should not be assumed to comply with the
--   variants of the ISO8601 standard. Only assume YYYYMMDDTHH:mm:SS\"
-- FIXME: make more robust
readDateTime :: Monad m => String -> Err m LocalTime
readDateTime dt =
    maybe
        (fail $ "Error parsing dateTime '" ++ dt ++ "'")
        return
        (parseTime defaultTimeLocale xmlRpcDateFormat dt)

localTimeToCalendarTime :: LocalTime -> CalendarTime
localTimeToCalendarTime l =
    let (y,mo,d) = toGregorian (localDay l)
        TimeOfDay { todHour = h, todMin = mi, todSec = s } = localTimeOfDay l
        (_,_,wd) = toWeekDate (localDay l)
        (_,yd) = toOrdinalDate (localDay l)
     in CalendarTime {
	              ctYear    = fromIntegral y,
		      ctMonth   = toEnum (mo-1),
		      ctDay     = d,
		      ctHour    = h,
		      ctMin     = mi,
		      ctSec     = truncate s,
		      ctPicosec = 0,
		      ctWDay    = toEnum (wd `mod` 7),
		      ctYDay    = yd,
		      ctTZName  = "UTC",
		      ctTZ      = 0,
		      ctIsDST   = False
		     }

calendarTimeToLocalTime :: CalendarTime -> LocalTime
calendarTimeToLocalTime ct =
    let (y,mo,d) = (ctYear ct, ctMonth ct, ctDay ct)
        (h,mi,s) = (ctHour ct, ctMin ct, ctSec ct)
     in LocalTime {
                   localDay = fromGregorian (fromIntegral y) (fromEnum mo + 1) d,
                   localTimeOfDay = TimeOfDay { todHour = h, todMin = mi, todSec = fromIntegral s }
                  }

-- FIXME: what if data contains non-base64 characters?
readBase64 :: Monad m => String -> Err m BS.ByteString
readBase64 = return . Base64.decode . BS.pack

fromXRParams :: Monad m => XR.Params -> Err m [Value]
fromXRParams (XR.Params xps) = mapM (\(XR.Param v) -> fromXRValue v) xps

fromXRMethodCall :: Monad m => XR.MethodCall -> Err m MethodCall
fromXRMethodCall (XR.MethodCall (XR.MethodName name) params) =
    liftM (MethodCall name) (fromXRParams (fromMaybe (XR.Params []) params))

fromXRMethodResponse :: Monad m => XR.MethodResponse -> Err m MethodResponse
fromXRMethodResponse (XR.MethodResponseParams xps) =
    liftM Return (fromXRParams xps >>= onlyOneResult)
fromXRMethodResponse (XR.MethodResponseFault (XR.Fault v)) =
    do
    struct <- fromXRValue v
    vcode <- structGetValue "faultCode" struct
    code <- fromValue vcode
    vstr <- structGetValue "faultString" struct
    str <- fromValue vstr
    return (Fault code str)

--
-- Parsing calls and reponses from XML
--

-- | Parses a method call from XML.
parseCall :: (Show e, MonadError e m) => String -> Err m MethodCall
parseCall c =
    do
    mxc <- errorToErr (readXml c)
    xc <- eitherToM "Error parsing method call" mxc
    fromXRMethodCall xc

-- | Parses a method response from XML.
parseResponse :: (Show e, MonadError e m) => String -> Err m MethodResponse
parseResponse c =
    do
    mxr <- errorToErr (readXml c)
    xr <- eitherToM "Error parsing method response" mxr
    fromXRMethodResponse xr

--
-- Rendering calls and reponses to XML
--

-- | Makes an XML-representation of a method call.
-- FIXME: pretty prints ugly XML
renderCall :: MethodCall -> BSL.ByteString
renderCall = showXml' False . toXRMethodCall

-- | Makes an XML-representation of a method response.
-- FIXME: pretty prints ugly XML
renderResponse :: MethodResponse -> BSL.ByteString
renderResponse  = showXml' False . toXRMethodResponse

showXml' :: XmlContent a => Bool -> a -> BSL.ByteString
showXml' dtd x = case toContents x of
                   [CElem _ _] -> (document . toXml dtd) x
                   _ -> BSL.pack ""
