{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module PrettyNoMin(renderResponseNoMin) where
-- Based on Pretty.hs from haxr,
-- changed to not minimize empty tags 
import Prelude hiding (maybe, elem, concat, null, head)
import qualified Prelude as P
import Data.ByteString.Lazy.Char8 (ByteString(), elem, empty)
import qualified Data.ByteString.Lazy.UTF8 as BU
import Text.XML.HaXml.Types
import Blaze.ByteString.Builder (Builder, fromLazyByteString, toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.Maybe (isNothing)
import Data.Monoid (Monoid, mempty, mconcat, mappend)
import qualified GHC.Exts as Ext
import Text.XML.HaXml.XmlContent(XmlContent(toContents),toXml)

import Network.XmlRpc.Internals(toXRMethodResponse)
import qualified Network.XmlRpc.DTD_XMLRPC as XR
import qualified Data.ByteString.Lazy.Char8 as BSL (ByteString, pack)

renderResponseNoMin  = showXml' False . toXRMethodResponse

showXml' :: XmlContent a => Bool -> a -> BSL.ByteString
showXml' dtd x = case toContents x of
                   [CElem _ _] -> (document . toXml dtd) x
                   _ -> BSL.pack ""
  where XmlPrinters{documentO=documentB} =
          fix (noMinimalElements . defaultMethods)
        document = runMBuilder . documentB

noMinimalElements :: XmlPrinters -> XmlPrinters
noMinimalElements printers@XmlPrinters{contentO=contentB} =
  printers {elementO=elementI}
  where
    elementI (Elem n as cs) 
      | isText (P.head cs)  = "<" <> (name n <+> fsep (map attribute as)) <> ">" <>
                              hcatMap contentB cs <> "</" <> name n <> ">"
      | otherwise           = "<" <> (name n <+> fsep (map attribute as)) <> ">" <>
                              hcatMap contentB cs <> "</" <> name n <> ">"

fix :: (a -> a) -> a
fix f = let x = f x in x

-- |A 'Builder' with a recognizable empty value.
newtype MBuilder = MBuilder { unMB :: Maybe Builder } deriving Monoid

-- |'Maybe' eliminator specialized for 'MBuilder'.
maybe :: (t -> MBuilder) -> Maybe t -> MBuilder
maybe _ Nothing = mempty
maybe f (Just x) = f x

-- |Nullity predicate for 'MBuilder'.
null :: MBuilder -> Bool
null = isNothing . unMB

-- |Helper for injecting 'ByteString's into 'MBuilder'.
fromLBS :: ByteString -> MBuilder
fromLBS = MBuilder . Just . fromLazyByteString

-- Helper needed when using Data.Binary.Builder.
-- fromString :: String -> Builder
-- fromString = fromLazyByteString . BU.fromString

-- |Support for the OverloadedStrings extension to improve templating
-- syntax.
instance Ext.IsString MBuilder where
  fromString "" = mempty
  fromString s = MBuilder . Just . fromString $ s

-- A simple implementation of the pretty-printing combinator interface,
-- but for plain ByteStrings:
infixr 6 <>
infixr 6 <+>
infixr 5 $$

-- |Beside.
(<>) :: MBuilder -> MBuilder -> MBuilder
(<>) = mappend

-- |Concatenate two 'MBuilder's with a single space in between
-- them. If either of the component 'MBuilder's is empty, then the
-- other is returned without any additional space.
(<+>) :: MBuilder -> MBuilder -> MBuilder
(<+>) b1 b2
  | null b2 = b1
  | null b1 = b2
  | otherwise = b1 <> " " <> b2

-- |Concatenate two 'MBuilder's with a single newline in between
-- them. If either of the component 'MBuilder's is empty, then the
-- other is returned without any additional newline.
($$) :: MBuilder -> MBuilder -> MBuilder
($$) b1 b2 
  | null b2 = b1
  | null b1 = b2
  | otherwise =  b1 <> "\n" <> b2

-- |Concatenate a list of 'MBuilder's with a given 'MBuilder' inserted
-- between each non-empty element of the list.
intercalate :: MBuilder -> [MBuilder] -> MBuilder
intercalate sep = aux . filter (not . null)
  where aux []     = mempty
        aux (x:xs) = x <> mconcat (map (sep <>) xs)

-- |List version of '<+>'.
hsep :: [MBuilder] -> MBuilder             
hsep = intercalate " "

-- |List version of '$$'.
vcat :: [MBuilder] -> MBuilder
vcat = intercalate "\n"

hcatMap :: (a -> MBuilder) -> [a] -> MBuilder
hcatMap = (mconcat .) . map

vcatMap :: (a -> MBuilder) -> [a] -> MBuilder
vcatMap = (vcat .) . map

-- |``Paragraph fill'' version of 'sep'.
fsep :: [MBuilder] -> MBuilder     
fsep = hsep

-- |Bracket an 'MBuilder' with parentheses.
parens :: MBuilder -> MBuilder
parens p = "(" <> p <> ")"

text :: String -> MBuilder
text = MBuilder . Just . fromString

name :: QName -> MBuilder
name = MBuilder . Just . fromString . unQ
  where unQ (QN (Namespace prefix uri) n) = prefix++":"++n
        unQ (N n) = n

----
-- Now for the XML pretty-printing interface.
-- (Basically copied direct from Text.XML.HaXml.Pretty).

{-
-- |Render a 'Document' to a 'ByteString'.
document    :: Document i  -> ByteString
content     :: Content i   -> ByteString
element     :: Element i   -> ByteString
doctypedecl :: DocTypeDecl -> ByteString
prolog      :: Prolog      -> ByteString
cp          :: CP          -> ByteString

-- Builder variants of exported functions.
documentB    :: Document i  -> MBuilder
contentB     :: Content i   -> MBuilder
elementB     :: Element i   -> MBuilder
doctypedeclB :: DocTypeDecl -> MBuilder
prologB      :: Prolog      -> MBuilder
cpB          :: CP          -> MBuilder
-}

xmldecl    :: XMLDecl    -> MBuilder
misc       :: Misc       -> MBuilder
sddecl     :: Bool       -> MBuilder
attribute  :: Attribute  -> MBuilder

-- |Run an 'MBuilder' to generate a 'ByteString'.
runMBuilder :: MBuilder -> ByteString
runMBuilder = aux . unMB
  where aux Nothing = empty
        aux (Just b) = toLazyByteString b

document    = runMBuilder . documentB
content     = runMBuilder . contentB
element     = runMBuilder . elementB
prolog      = runMBuilder . prologB
doctypedecl = runMBuilder . doctypedeclB
cp          = runMBuilder . cpB

data XmlPrinters = XmlPrinters
  { documentO    :: forall i . Document i  -> MBuilder
  , prologO      :: Prolog      -> MBuilder
  , doctypedeclO :: DocTypeDecl -> MBuilder
  , elementO     :: forall i . Element i   -> MBuilder
  , contentO     :: forall i . Content i   -> MBuilder
  , cpO          :: CP          -> MBuilder
  }

XmlPrinters{documentO=documentB,
            prologO=prologB,
            doctypedeclO=doctypedeclB,
            elementO=elementB,
            contentO=contentB,
            cpO=cpB} = defaults
  where defaults = defaultMethods defaults

defaultMethods :: XmlPrinters -> XmlPrinters
defaultMethods XmlPrinters{documentO=documentB,
                           prologO=prologB,
                           elementO=elementB,
                           doctypedeclO=doctypedeclB,
                           contentO=contentB,
                           cpO=cpB} =
  XmlPrinters{documentO=documentI,
              prologO=prologI,
              doctypedeclO=doctypedeclI,
              elementO=elementI,
              contentO=contentI,
              cpO=cpI}
  where
    documentI (Document p _ e m) = prologB p $$ elementB e $$ vcatMap misc m

    prologI (Prolog x m1 dtd m2) = maybe xmldecl x $$
                                   vcatMap misc m1 $$
                                   maybe doctypedeclB dtd $$
                                   vcatMap misc m2

    doctypedeclI (DTD n eid ds)  = if P.null ds then hd <> ">"
                                   else hd <+> " [" $$ vcatMap markupdecl ds $$ "]>"
      where hd = "<!DOCTYPE" <+> name n <+> maybe externalid eid

    markupdecl :: MarkupDecl -> MBuilder
    markupdecl (Element e)      = elementdecl e
    markupdecl (AttList a)      = attlistdecl a
    markupdecl (Entity e)       = entitydecl e
    markupdecl (Notation n)     = notationdecl n
    markupdecl (MarkupMisc m)   = misc m

    elementI (Elem n as []) = "<" <> (name n <+> fsep (map attribute as)) <> "/>"
    elementI (Elem n as cs) 
      | isText (P.head cs)  = "<" <> (name n <+> fsep (map attribute as)) <> ">" <>
                              hcatMap contentB cs <> "</" <> name n <> ">"
      | otherwise           = "<" <> (name n <+> fsep (map attribute as)) <> ">" <>
                              hcatMap contentB cs <> "</" <> name n <> ">"

    contentI (CElem e _)         = elementB e
    contentI (CString False s _) = chardata s
    contentI (CString True s _)  = cdsect s
    contentI (CRef r _)          = reference r
    contentI (CMisc m _)         = misc m

    elementdecl :: ElementDecl -> MBuilder
    elementdecl (ElementDecl n cs) = "<!ELEMENT" <+> name n <+>
                                     contentspec cs <> ">"

    contentspec :: ContentSpec -> MBuilder
    contentspec EMPTY           = "EMPTY"
    contentspec ANY             = "ANY"
    contentspec (Mixed m)       = mixed m
    contentspec (ContentSpec c) = cpB c

    cpI (TagName n m) = name n <> modifier m
    cpI (Choice cs m) = parens (intercalate "|" (map cpB cs)) <> modifier m
    cpI (Seq cs m)    = parens (intercalate "," (map cpB cs)) <> modifier m

xmldecl (XMLDecl v e sd)    = "<?xml version='" <> text v <> "'" <+>
                              maybe encodingdecl e <+>
                              maybe sddecl sd <+> "?>"

misc (Comment s)            = "<!--" <+> text s <+> "-->"
misc (PI (n,s))             = "<?" <> text n <+> text s <+> "?>"

sddecl sd   | sd            = "standalone='yes'"
            | otherwise     = "standalone='no'"

isText :: Content t -> Bool
isText (CString _ _ _) = True
isText (CRef _ _)      = True
isText _               = False

attribute (n,v) = name n <> "=" <> attvalue v

modifier :: Modifier -> MBuilder
modifier None  = mempty
modifier Query = "?"
modifier Star  = "*"
modifier Plus  = "+"

mixed :: Mixed -> MBuilder
mixed  PCDATA         = "(#PCDATA)"
mixed (PCDATAplus ns) = "(#PCDATA |" <+> intercalate "|" (map name ns) <> ")*"

attlistdecl :: AttListDecl -> MBuilder
attlistdecl (AttListDecl n ds) = "<!ATTLIST" <+> name n <+> 
                                 fsep (map attdef ds) <> ">"

attdef :: AttDef -> MBuilder
attdef (AttDef n t d)          = name n <+> atttype t <+> defaultdecl d

atttype :: AttType -> MBuilder
atttype  StringType            = "CDATA"
atttype (TokenizedType t)      = tokenizedtype t
atttype (EnumeratedType t)     = enumeratedtype t

tokenizedtype :: TokenizedType -> MBuilder
tokenizedtype ID               = "ID"
tokenizedtype IDREF            = "IDREF"
tokenizedtype IDREFS           = "IDREFS"
tokenizedtype ENTITY           = "ENTITY"
tokenizedtype ENTITIES         = "ENTITIES"
tokenizedtype NMTOKEN          = "NMTOKEN"
tokenizedtype NMTOKENS         = "NMTOKENS"

enumeratedtype :: EnumeratedType -> MBuilder
enumeratedtype (NotationType n) = notationtype n
enumeratedtype (Enumeration e)  = enumeration e

notationtype :: [[Char]] -> MBuilder
notationtype ns                = "NOTATION" <+>
                                 parens (intercalate "|" (map text ns))

enumeration :: [[Char]] -> MBuilder
enumeration ns                 = parens (intercalate "|" (map nmtoken ns))

defaultdecl :: DefaultDecl -> MBuilder
defaultdecl  REQUIRED          = "#REQUIRED"
defaultdecl  IMPLIED           = "#IMPLIED"
defaultdecl (DefaultTo a f)    = maybe (const "#FIXED") f <+> attvalue a

reference :: Reference -> MBuilder
reference (RefEntity er)       = entityref er
reference (RefChar cr)         = charref cr

entityref :: [Char] -> MBuilder
entityref n                    = "&" <> text n <> ";"

charref :: (Show a) => a -> MBuilder
charref c                      = "&#" <> text (show c) <> ";"

entitydecl :: EntityDecl -> MBuilder
entitydecl (EntityGEDecl d)    = gedecl d
entitydecl (EntityPEDecl d)    = pedecl d

gedecl :: GEDecl -> MBuilder
gedecl (GEDecl n ed)           = "<!ENTITY" <+> text n <+> entitydef ed <> ">"

pedecl :: PEDecl -> MBuilder
pedecl (PEDecl n pd)           = "<!ENTITY %" <> text n <+> pedef pd <> ">"

entitydef :: EntityDef -> MBuilder
entitydef (DefEntityValue ew)  = entityvalue ew
entitydef (DefExternalID i nd) = externalid i <+> maybe ndatadecl nd

pedef :: PEDef -> MBuilder
pedef (PEDefEntityValue ew)    = entityvalue ew
pedef (PEDefExternalID eid)    = externalid eid

externalid :: ExternalID -> MBuilder
externalid (SYSTEM sl)         = "SYSTEM" <+> systemliteral sl
externalid (PUBLIC i sl)       = "PUBLIC" <+> pubidliteral i <+> systemliteral sl

ndatadecl :: NDataDecl -> MBuilder
ndatadecl (NDATA n)            = "NDATA" <+> text n

notationdecl :: NotationDecl -> MBuilder
notationdecl (NOTATION n e)    = "<!NOTATION" <+> text n <+>
                                 either externalid publicid e <> ">"

publicid :: PublicID -> MBuilder
publicid (PUBLICID p)          = "PUBLICID" <+> pubidliteral p

encodingdecl :: EncodingDecl -> MBuilder
encodingdecl (EncodingDecl s)  = "encoding='" <> text s <> "'"

nmtoken :: [Char] -> MBuilder
nmtoken s                      = text s

attvalue :: AttValue -> MBuilder
attvalue (AttValue esr)        = "\"" <> hcatMap attVal esr <> "\""
  where attVal = either text reference

entityvalue :: EntityValue -> MBuilder
entityvalue (EntityValue evs)
  | containsDoubleQuote evs    = "'"  <> hcatMap ev evs <> "'"
  | otherwise                  = "\"" <> hcatMap ev evs <> "\""

ev :: EV -> MBuilder
ev (EVString s)                = text s
ev (EVRef r)                   = reference r

pubidliteral :: PubidLiteral -> MBuilder
pubidliteral (PubidLiteral s)
    | '"' `elem` s' = "'" <> fromLBS s' <> "'"
    | otherwise     = "\"" <> fromLBS s' <> "\""
    where s' = BU.fromString s

systemliteral :: SystemLiteral -> MBuilder
systemliteral (SystemLiteral s)
    | '"' `elem` s' = "'" <> fromLBS s' <> "'"
    | otherwise     = "\"" <> fromLBS s' <> "\""
    where s' = BU.fromString s

chardata, cdsect :: [Char] -> MBuilder
chardata s                     = {-if all isSpace s then empty else-} text s
cdsect c                       = "<![CDATA[" <> chardata c <> "]]>"

containsDoubleQuote :: [EV] -> Bool
containsDoubleQuote evs = any csq evs
    where csq (EVString s) = '"' `elem` BU.fromString s
          csq _            = False
