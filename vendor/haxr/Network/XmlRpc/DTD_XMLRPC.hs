module Network.XmlRpc.DTD_XMLRPC where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN
import Text.XML.HaXml.Types (QName(..))


{-Type decls-}

newtype I4 = I4 String 		deriving (Eq,Show)
newtype I8 = I8 String 		deriving (Eq,Show)
newtype AInt = AInt String 		deriving (Eq,Show)
newtype Boolean = Boolean String 		deriving (Eq,Show)
newtype AString = AString String 		deriving (Eq,Show)
newtype ADouble = ADouble String 		deriving (Eq,Show)
newtype DateTime_iso8601 = DateTime_iso8601 String 		deriving (Eq,Show)
newtype Base64 = Base64 String 		deriving (Eq,Show)
newtype Data = Data [Value] 		deriving (Eq,Show)
newtype Array = Array Data 		deriving (Eq,Show)
newtype Name = Name String 		deriving (Eq,Show)
data Member = Member Name Value
            deriving (Eq,Show)
newtype Struct = Struct [Member] 		deriving (Eq,Show)
newtype Value = Value [Value_] 		deriving (Eq,Show)
data Value_ = Value_Str String
            | Value_I4 I4
            | Value_I8 I8
            | Value_AInt AInt
            | Value_Boolean Boolean
            | Value_AString AString
            | Value_DateTime_iso8601 DateTime_iso8601
            | Value_ADouble ADouble
            | Value_Base64 Base64
            | Value_Struct Struct
            | Value_Array Array
            deriving (Eq,Show)
newtype Param = Param Value 		deriving (Eq,Show)
newtype Params = Params [Param] 		deriving (Eq,Show)
newtype MethodName = MethodName String 		deriving (Eq,Show)
data MethodCall = MethodCall MethodName (Maybe Params)
                deriving (Eq,Show)
newtype Fault = Fault Value 		deriving (Eq,Show)
data MethodResponse = MethodResponseParams Params
                    | MethodResponseFault Fault
                    deriving (Eq,Show)


{-Instance decls-}

instance HTypeable I4 where
    toHType x = Defined "i4" [] []
instance XmlContent I4 where
    toContents (I4 a) =
        [CElem (Elem (N "i4") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["i4"]
        ; interior e $ return (I4) `apply` (text `onFail` return "")
        } `adjustErr` ("in <i4>, "++)

instance HTypeable I8 where
    toHType x = Defined "i8" [] []
instance XmlContent I8 where
    toContents (I8 a) =
        [CElem (Elem (N "i8") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["i8"]
        ; interior e $ return (I8) `apply` (text `onFail` return "")
        } `adjustErr` ("in <i8>, "++)

instance HTypeable AInt where
    toHType x = Defined "int" [] []
instance XmlContent AInt where
    toContents (AInt a) =
        [CElem (Elem (N "int") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["int"]
        ; interior e $ return (AInt) `apply` (text `onFail` return "")
        } `adjustErr` ("in <int>, "++)

instance HTypeable Boolean where
    toHType x = Defined "boolean" [] []
instance XmlContent Boolean where
    toContents (Boolean a) =
        [CElem (Elem (N "boolean") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["boolean"]
        ; interior e $ return (Boolean) `apply` (text `onFail` return "")
        } `adjustErr` ("in <boolean>, "++)

instance HTypeable AString where
    toHType x = Defined "string" [] []
instance XmlContent AString where
    toContents (AString a) =
        [CElem (Elem (N "string") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["string"]
        ; interior e $ return (AString) `apply` (text `onFail` return "")
        } `adjustErr` ("in <string>, "++)

instance HTypeable ADouble where
    toHType x = Defined "double" [] []
instance XmlContent ADouble where
    toContents (ADouble a) =
        [CElem (Elem (N "double") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["double"]
        ; interior e $ return (ADouble) `apply` (text `onFail` return "")
        } `adjustErr` ("in <double>, "++)

instance HTypeable DateTime_iso8601 where
    toHType x = Defined "dateTime.iso8601" [] []
instance XmlContent DateTime_iso8601 where
    toContents (DateTime_iso8601 a) =
        [CElem (Elem (N "dateTime.iso8601") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["dateTime.iso8601"]
        ; interior e $ return (DateTime_iso8601)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <dateTime.iso8601>, "++)

instance HTypeable Base64 where
    toHType x = Defined "base64" [] []
instance XmlContent Base64 where
    toContents (Base64 a) =
        [CElem (Elem (N "base64") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["base64"]
        ; interior e $ return (Base64) `apply` (text `onFail` return "")
        } `adjustErr` ("in <base64>, "++)

instance HTypeable Data where
    toHType x = Defined "data" [] []
instance XmlContent Data where
    toContents (Data a) =
        [CElem (Elem (N "data") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["data"]
        ; interior e $ return (Data) `apply` many parseContents
        } `adjustErr` ("in <data>, "++)

instance HTypeable Array where
    toHType x = Defined "array" [] []
instance XmlContent Array where
    toContents (Array a) =
        [CElem (Elem (N "array") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["array"]
        ; interior e $ return (Array) `apply` parseContents
        } `adjustErr` ("in <array>, "++)

instance HTypeable Name where
    toHType x = Defined "name" [] []
instance XmlContent Name where
    toContents (Name a) =
        [CElem (Elem (N "name") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["name"]
        ; interior e $ return (Name) `apply` (text `onFail` return "")
        } `adjustErr` ("in <name>, "++)

instance HTypeable Member where
    toHType x = Defined "member" [] []
instance XmlContent Member where
    toContents (Member a b) =
        [CElem (Elem (N "member") [] (toContents a ++ toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["member"]
        ; interior e $ return (Member) `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <member>, "++)

instance HTypeable Struct where
    toHType x = Defined "struct" [] []
instance XmlContent Struct where
    toContents (Struct a) =
        [CElem (Elem (N "struct") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["struct"]
        ; interior e $ return (Struct) `apply` many parseContents
        } `adjustErr` ("in <struct>, "++)

instance HTypeable Value where
    toHType x = Defined "value" [] []
instance XmlContent Value where
    toContents (Value a) =
        [CElem (Elem (N "value") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["value"]
        ; interior e $ return (Value) `apply` many parseContents
        } `adjustErr` ("in <value>, "++)

instance HTypeable Value_ where
    toHType x = Defined "value" [] []
instance XmlContent Value_ where
    toContents (Value_Str a) = toText a
    toContents (Value_I4 a) = toContents a
    toContents (Value_I8 a) = toContents a
    toContents (Value_AInt a) = toContents a
    toContents (Value_Boolean a) = toContents a
    toContents (Value_AString a) = toContents a
    toContents (Value_DateTime_iso8601 a) = toContents a
    toContents (Value_ADouble a) = toContents a
    toContents (Value_Base64 a) = toContents a
    toContents (Value_Struct a) = toContents a
    toContents (Value_Array a) = toContents a
    parseContents = oneOf
        [ return (Value_Str) `apply` text
        , return (Value_I4) `apply` parseContents
        , return (Value_I8) `apply` parseContents
        , return (Value_AInt) `apply` parseContents
        , return (Value_Boolean) `apply` parseContents
        , return (Value_AString) `apply` parseContents
        , return (Value_DateTime_iso8601) `apply` parseContents
        , return (Value_ADouble) `apply` parseContents
        , return (Value_Base64) `apply` parseContents
        , return (Value_Struct) `apply` parseContents
        , return (Value_Array) `apply` parseContents
        ] `adjustErr` ("in <value>, "++)

instance HTypeable Param where
    toHType x = Defined "param" [] []
instance XmlContent Param where
    toContents (Param a) =
        [CElem (Elem (N "param") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["param"]
        ; interior e $ return (Param) `apply` parseContents
        } `adjustErr` ("in <param>, "++)

instance HTypeable Params where
    toHType x = Defined "params" [] []
instance XmlContent Params where
    toContents (Params a) =
        [CElem (Elem (N "params") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["params"]
        ; interior e $ return (Params) `apply` many parseContents
        } `adjustErr` ("in <params>, "++)

instance HTypeable MethodName where
    toHType x = Defined "methodName" [] []
instance XmlContent MethodName where
    toContents (MethodName a) =
        [CElem (Elem (N "methodName") [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["methodName"]
        ; interior e $ return (MethodName)
                       `apply` (text `onFail` return "")
        } `adjustErr` ("in <methodName>, "++)

instance HTypeable MethodCall where
    toHType x = Defined "methodCall" [] []
instance XmlContent MethodCall where
    toContents (MethodCall a b) =
        [CElem (Elem (N "methodCall") [] (toContents a ++
                                      maybe [] toContents b)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["methodCall"]
        ; interior e $ return (MethodCall) `apply` parseContents
                       `apply` optional parseContents
        } `adjustErr` ("in <methodCall>, "++)

instance HTypeable Fault where
    toHType x = Defined "fault" [] []
instance XmlContent Fault where
    toContents (Fault a) =
        [CElem (Elem (N "fault") [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["fault"]
        ; interior e $ return (Fault) `apply` parseContents
        } `adjustErr` ("in <fault>, "++)

instance HTypeable MethodResponse where
    toHType x = Defined "methodResponse" [] []
instance XmlContent MethodResponse where
    toContents (MethodResponseParams a) =
        [CElem (Elem (N "methodResponse") [] (toContents a) ) ()]
    toContents (MethodResponseFault a) =
        [CElem (Elem (N "methodResponse") [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["methodResponse"]
        ; interior e $ oneOf
            [ return (MethodResponseParams) `apply` parseContents
            , return (MethodResponseFault) `apply` parseContents
            ] `adjustErr` ("in <methodResponse>, "++)
        }



{-Done-}
