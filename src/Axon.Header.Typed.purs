module Axon.Header.Typed where

import Prelude

import Axon.Request.Method (Method)
import Axon.Request.Method as Method
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Either.Nested as Either.Nested
import Data.Int as Int
import Data.MIME as MIME
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.String.Base64 as String.Base64
import Data.String.CodeUnits as String.CodeUnit
import Data.String.Lower (StringLower)
import Data.String.Lower as String.Lower
import Data.String.Regex.Flags as Regex.Flags
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception as Error
import Parsing (Parser)
import Parsing as Parse
import Parsing.Combinators as Parse.Combine
import Parsing.String as Parse.String
import Parsing.String.Basic as Parse.String.Basic
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Nub, class Union)
import Record as Record
import Type.MIME as Type.MIME

data Wildcard = Wildcard

commas :: forall a. Parser String a -> Parser String (Array a)
commas p = Parse.Combine.sepBy p (Parse.String.Basic.whiteSpace <* Parse.String.string "," <* Parse.String.Basic.whiteSpace) <#> Array.fromFoldable

commas1 :: forall a. Parser String a -> Parser String (NonEmptyArray a)
commas1 p = Parse.Combine.sepBy1 p (Parse.String.Basic.whiteSpace <* Parse.String.string "," <* Parse.String.Basic.whiteSpace) <#> Array.NonEmpty.fromFoldable1

wildcardParser :: Parser String Wildcard
wildcardParser = Parse.String.string "*" $> Wildcard

mimeParser :: Parser String MIME.MIME
mimeParser = Parse.String.anyTill Parse.String.Basic.space <#> fst <#> MIME.fromString

headerNameRegexParser :: Parser String String
headerNameRegexParser = unsafePartial $ (\(Right a) -> a) $ Parse.String.regex "[\\w-]+" Regex.Flags.noFlags

headerNameParser :: Parser String StringLower
headerNameParser = headerNameRegexParser <#> String.Lower.fromString

methodParser :: Parser String Method
methodParser = Parse.Combine.many Parse.String.Basic.alphaNum <#> Array.fromFoldable <#> String.CodeUnit.fromCharArray >>= (\a -> Parse.liftMaybe (const $ "invalid method " <> a) $ Method.fromString a)

directiveParser :: Parser String (String /\ Maybe String)
directiveParser =
  let
    boundary = Parse.String.string ";" <|> Parse.String.string "," <|> (Parse.String.eof *> pure "")
    kvParser = do
      k /\ stop <- Parse.String.anyTill (Parse.String.string "=" <|> boundary)
      when (stop /= "=") $ Parse.fail ""
      v <- Parse.String.anyTill boundary <#> fst
      pure $ String.trim k /\ Just (String.trim v)
    kParser = Parse.String.anyTill boundary <#> fst <#> String.trim <#> (\k -> k /\ Nothing)
  in
    kvParser <|> kParser

class TypedHeader a where
  headerName :: String
  headerValueParser :: Parser String a
  headerValueEncode :: a -> String

newtype Accept a = Accept a
derive instance Newtype (Accept a) _

data AccessControlAllowCredentials = AccessControlAllowCredentials

newtype AccessControlAllowHeaders = AccessControlAllowHeaders (Wildcard \/ NonEmptyArray StringLower)
derive instance Newtype (AccessControlAllowHeaders) _

newtype AccessControlAllowMethods = AccessControlAllowMethods (Wildcard \/ NonEmptyArray Method)
derive instance Newtype (AccessControlAllowMethods) _

newtype AccessControlAllowOrigin = AccessControlAllowOrigin (Wildcard \/ String)
derive instance Newtype (AccessControlAllowOrigin) _

newtype AccessControlExposeHeaders = AccessControlExposeHeaders (Wildcard \/ Array StringLower)
derive instance Newtype (AccessControlExposeHeaders) _

newtype AccessControlMaxAge = AccessControlMaxAge Int
derive instance Newtype (AccessControlMaxAge) _

newtype AccessControlRequestHeaders = AccessControlRequestHeaders (NonEmptyArray StringLower)
derive instance Newtype (AccessControlRequestHeaders) _

newtype AccessControlRequestMethod = AccessControlRequestMethod Method
derive instance Newtype (AccessControlRequestMethod) _

newtype Age = Age Int
derive instance Newtype (Age) _

newtype Allow = Allow (NonEmptyArray Method)
derive instance Newtype (Allow) _

newtype AuthScheme = AuthScheme String
derive instance Newtype (AuthScheme) _

data Authorization = Authorization AuthScheme String

newtype BearerAuth = BearerAuth String
derive instance Newtype (BearerAuth) _

newtype BasicAuth = BasicAuth {username :: String, password :: String}
derive instance Newtype (BasicAuth) _

newtype ByteRangeStart = ByteRangeStart Int
derive instance Newtype (ByteRangeStart) _

newtype ByteRangeEnd = ByteRangeEnd Int
derive instance Newtype (ByteRangeEnd) _

newtype ByteRangeLength = ByteRangeLength Int
derive instance Newtype (ByteRangeLength) _

type CacheControl' =
  ( maxAge :: Maybe Int
  , maxStale :: Maybe Int
  , minFresh :: Maybe Int
  , sMaxAge :: Maybe Int
  , noCache :: Boolean
  , noStore :: Boolean
  , noTransform :: Boolean
  , onlyIfCached :: Boolean
  , mustRevalidate :: Boolean
  , mustUnderstand :: Boolean
  , proxyRevalidate :: Boolean
  , private :: Boolean
  , public :: Boolean
  , immutable :: Boolean
  , staleWhileRevalidate :: Boolean
  , staleIfError :: Boolean
  )

newtype CacheControl = CacheControl (Record CacheControl')
derive instance Newtype (CacheControl) _

data CloseConnection = CloseConnection

newtype Connection = Connection (CloseConnection \/ NonEmptyArray StringLower)
derive instance Newtype (Connection) _

newtype ContentDisposition = ContentDisposition (ContentDispositionInline \/ ContentDispositionAttachment \/ ContentDispositionFormData)
derive instance Newtype (ContentDisposition) _

data ContentDispositionInline = ContentDispositionInline

newtype ContentDispositionAttachment = ContentDispositionAttachment {filename :: Maybe {language :: Maybe String, encoding :: Maybe String, value :: String}}
derive instance Newtype (ContentDispositionAttachment) _

newtype ContentDispositionFormData = ContentDispositionFormData {filename :: Maybe String, name :: Maybe String}
derive instance Newtype (ContentDispositionFormData) _

newtype ContentEncoding = ContentEncoding (NonEmptyArray String)
derive instance Newtype (ContentEncoding) _

newtype ContentLength = ContentLength Int
derive instance Newtype (ContentLength) _

newtype ContentLocation = ContentLocation String
derive instance Newtype (ContentLocation) _

newtype ContentRange = ContentRange ((ByteRangeStart /\ ByteRangeEnd /\ ByteRangeLength) \/ (ByteRangeStart /\ ByteRangeEnd) \/ ByteRangeLength)
derive instance Newtype (ContentRange) _

newtype ContentType a = ContentType a
derive instance Newtype (ContentType a) _

newtype Cookie = Cookie String
derive instance Newtype (Cookie) _

newtype Date = Date DateTime
derive instance Newtype (Date) _

newtype ETag = ETag String
derive instance Newtype (ETag) _

data ExpectContinue = ExpectContinue
newtype Expires = Expires DateTime
derive instance Newtype (Expires) _

newtype Host = Host String
derive instance Newtype (Host) _

newtype IfMatch = IfMatch (Wildcard \/ NonEmptyArray MatchETag)
derive instance Newtype (IfMatch) _

newtype IfNoneMatch = IfNoneMatch (Wildcard \/ NonEmptyArray MatchETag)
derive instance Newtype (IfNoneMatch) _

newtype IfModifiedSince = IfModifiedSince DateTime
derive instance Newtype (IfModifiedSince) _

newtype IfRange = IfRange (DateTime \/ String)
derive instance Newtype (IfRange) _

newtype IfUnmodifiedSince = IfUnmodifiedSince DateTime
derive instance Newtype (IfUnmodifiedSince) _

newtype LastModified = LastModified DateTime
derive instance Newtype (LastModified) _

data MatchETag = MatchETag String | MatchETagWeak String

newtype Origin = Origin String
derive instance Newtype (Origin) _

data ProxyAuthorization = ProxyAuthorization AuthScheme String

type RangeSpecifier = ByteRangeStart \/ (ByteRangeStart /\ ByteRangeEnd) \/ ByteRangeLength

newtype Range = Range (RangeSpecifier \/ Array RangeSpecifier)
derive instance Newtype (Range) _

newtype Referer = Referer String
derive instance Newtype (Referer) _

data ReferrerPolicy
  = ReferrerPolicyNoReferrer
  | ReferrerPolicyNoReferrerWhenDowngrade
  | ReferrerPolicySameOrigin
  | ReferrerPolicyOrigin
  | ReferrerPolicyOriginWhenCrossOrigin
  | ReferrerPolicyUnsafeURL
  | ReferrerPolicyStrictOrigin
  | ReferrerPolicyStrictOriginWhenCrossOrigin

newtype RetryAfter = RetryAfter (DateTime \/ Int)
derive instance Newtype (RetryAfter) _

newtype SecWebsocketKey = SecWebsocketKey String
derive instance Newtype (SecWebsocketKey) _

newtype SecWebsocketAccept = SecWebsocketAccept SecWebsocketKey
derive instance Newtype (SecWebsocketAccept) _

newtype SecWebsocketVersion = SecWebsocketVersion (String \/ NonEmptyArray String)
derive instance Newtype (SecWebsocketVersion) _

newtype Server = Server String
derive instance Newtype (Server) _

newtype SetCookie = SetCookie String
derive instance Newtype (SetCookie) _

newtype StrictTransportSecurity = StrictTransportSecurity {maxAge :: Int, includeSubdomains :: Boolean, preload :: Boolean}
derive instance Newtype (StrictTransportSecurity) _

newtype TE = TE String
derive instance Newtype (TE) _

newtype TransferEncoding = TransferEncoding String
derive instance Newtype (TransferEncoding) _

newtype Upgrade = Upgrade (NonEmptyArray String)
derive instance Newtype (Upgrade) _

newtype UserAgent = UserAgent String
derive instance Newtype (UserAgent) _

newtype Vary = Vary (Wildcard \/ NonEmptyArray StringLower)
derive instance Newtype (Vary) _

cacheControlDefaults :: Record CacheControl'
cacheControlDefaults =
  { maxAge: Nothing
  , maxStale: Nothing
  , minFresh: Nothing
  , sMaxAge: Nothing
  , noCache: false
  , noStore: false
  , noTransform: false
  , onlyIfCached: false
  , mustRevalidate: false
  , mustUnderstand: false
  , proxyRevalidate: false
  , private: false
  , public: false
  , immutable: false
  , staleWhileRevalidate: false
  , staleIfError: false
  }

cacheControl :: forall missing r withDefaults. Nub withDefaults CacheControl' => Union r CacheControl' withDefaults => Union missing r CacheControl' => Record r -> CacheControl
cacheControl a = CacheControl $ Record.merge a cacheControlDefaults

instance TypedHeader (Accept String) where
  headerName = "Accept"
  headerValueParser = Parse.String.rest <#> Accept
  headerValueEncode = unwrap
instance TypedHeader (Accept MIME.MIME) where
  headerName = "Accept"
  headerValueParser = mimeParser <#> Accept
  headerValueEncode = MIME.toString <<< unwrap
instance TypedHeader (Accept Type.MIME.Aac) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Aac)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Aac)
instance TypedHeader (Accept Type.MIME.Abw) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Abw)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Abw)
instance TypedHeader (Accept Type.MIME.Arc) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Arc)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Arc)
instance TypedHeader (Accept Type.MIME.Avif) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Avif)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Avif)
instance TypedHeader (Accept Type.MIME.Avi) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Avi)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Avi)
instance TypedHeader (Accept Type.MIME.Azw) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Azw)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Azw)
instance TypedHeader (Accept Type.MIME.Bin) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bin)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bin)
instance TypedHeader (Accept Type.MIME.Bmp) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bmp)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bmp)
instance TypedHeader (Accept Type.MIME.Bz) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bz)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bz)
instance TypedHeader (Accept Type.MIME.Bz2) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bz2)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bz2)
instance TypedHeader (Accept Type.MIME.Cda) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Cda)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Cda)
instance TypedHeader (Accept Type.MIME.Csh) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Csh)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Csh)
instance TypedHeader (Accept Type.MIME.Css) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Css)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Css)
instance TypedHeader (Accept Type.MIME.Csv) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Csv)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Csv)
instance TypedHeader (Accept Type.MIME.Doc) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Doc)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Doc)
instance TypedHeader (Accept Type.MIME.Docx) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Docx)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Docx)
instance TypedHeader (Accept Type.MIME.Eot) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Eot)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Eot)
instance TypedHeader (Accept Type.MIME.Epub) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Epub)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Epub)
instance TypedHeader (Accept Type.MIME.Gz) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Gz)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Gz)
instance TypedHeader (Accept Type.MIME.Gif) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Gif)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Gif)
instance TypedHeader (Accept Type.MIME.Html) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Html)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Html)
instance TypedHeader (Accept Type.MIME.Ico) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ico)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ico)
instance TypedHeader (Accept Type.MIME.Ics) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ics)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ics)
instance TypedHeader (Accept Type.MIME.Jar) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Jar)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Jar)
instance TypedHeader (Accept Type.MIME.Jpeg) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Jpeg)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Jpeg)
instance TypedHeader (Accept Type.MIME.Js) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Js)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Js)
instance TypedHeader (Accept Type.MIME.Json) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Json)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Json)
instance TypedHeader (Accept Type.MIME.Jsonld) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Jsonld)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Jsonld)
instance TypedHeader (Accept Type.MIME.Midi) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Midi)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Midi)
instance TypedHeader (Accept Type.MIME.Mjs) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mjs)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mjs)
instance TypedHeader (Accept Type.MIME.Mp3) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mp3)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mp3)
instance TypedHeader (Accept Type.MIME.Mp4) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mp4)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mp4)
instance TypedHeader (Accept Type.MIME.Mpeg) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mpeg)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mpeg)
instance TypedHeader (Accept Type.MIME.Mpkg) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mpkg)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mpkg)
instance TypedHeader (Accept Type.MIME.Odp) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Odp)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Odp)
instance TypedHeader (Accept Type.MIME.Ods) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ods)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ods)
instance TypedHeader (Accept Type.MIME.Odt) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Odt)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Odt)
instance TypedHeader (Accept Type.MIME.Oga) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Oga)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Oga)
instance TypedHeader (Accept Type.MIME.Ogv) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ogv)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ogv)
instance TypedHeader (Accept Type.MIME.Ogx) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ogx)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ogx)
instance TypedHeader (Accept Type.MIME.Opus) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Opus)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Opus)
instance TypedHeader (Accept Type.MIME.Otf) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Otf)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Otf)
instance TypedHeader (Accept Type.MIME.Png) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Png)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Png)
instance TypedHeader (Accept Type.MIME.Pdf) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Pdf)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Pdf)
instance TypedHeader (Accept Type.MIME.Php) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Php)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Php)
instance TypedHeader (Accept Type.MIME.Ppt) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ppt)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ppt)
instance TypedHeader (Accept Type.MIME.Pptx) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Pptx)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Pptx)
instance TypedHeader (Accept Type.MIME.Rar) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Rar)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Rar)
instance TypedHeader (Accept Type.MIME.Rtf) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Rtf)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Rtf)
instance TypedHeader (Accept Type.MIME.Sh) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Sh)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Sh)
instance TypedHeader (Accept Type.MIME.Svg) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Svg)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Svg)
instance TypedHeader (Accept Type.MIME.Tar) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Tar)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Tar)
instance TypedHeader (Accept Type.MIME.Tif) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Tif)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Tif)
instance TypedHeader (Accept Type.MIME.Ts) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ts)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ts)
instance TypedHeader (Accept Type.MIME.Ttf) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ttf)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ttf)
instance TypedHeader (Accept Type.MIME.Txt) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Txt)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Txt)
instance TypedHeader (Accept Type.MIME.Vsd) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Vsd)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Vsd)
instance TypedHeader (Accept Type.MIME.Wav) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Wav)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Wav)
instance TypedHeader (Accept Type.MIME.Weba) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Weba)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Weba)
instance TypedHeader (Accept Type.MIME.Webm) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Webm)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Webm)
instance TypedHeader (Accept Type.MIME.Webp) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Webp)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Webp)
instance TypedHeader (Accept Type.MIME.Woff) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Woff)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Woff)
instance TypedHeader (Accept Type.MIME.Woff2) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Woff2)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Woff2)
instance TypedHeader (Accept Type.MIME.Xhtml) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xhtml)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xhtml)
instance TypedHeader (Accept Type.MIME.Xls) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xls)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xls)
instance TypedHeader (Accept Type.MIME.Xlsx) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xlsx)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xlsx)
instance TypedHeader (Accept Type.MIME.Xml) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xml)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xml)
instance TypedHeader (Accept Type.MIME.Xul) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xul)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xul)
instance TypedHeader (Accept Type.MIME.Zip) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Zip)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Zip)
instance TypedHeader (Accept Type.MIME.Video3gp) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Video3gp)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Video3gp)
instance TypedHeader (Accept Type.MIME.Video3g2) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Video3g2)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Video3g2)
instance TypedHeader (Accept Type.MIME.Archive7z) where
  headerName = "Accept"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Archive7z)) <<< Type.MIME.fromValue) <#> Accept
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Archive7z)

instance TypedHeader (ContentType String) where
  headerName = "Content-Type"
  headerValueParser = Parse.String.rest <#> ContentType
  headerValueEncode = unwrap
instance TypedHeader (ContentType MIME.MIME) where
  headerName = "Content-Type"
  headerValueParser = mimeParser <#> ContentType
  headerValueEncode = MIME.toString <<< unwrap
instance TypedHeader (ContentType Type.MIME.Aac) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Aac)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Aac)
instance TypedHeader (ContentType Type.MIME.Abw) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Abw)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Abw)
instance TypedHeader (ContentType Type.MIME.Arc) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Arc)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Arc)
instance TypedHeader (ContentType Type.MIME.Avif) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Avif)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Avif)
instance TypedHeader (ContentType Type.MIME.Avi) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Avi)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Avi)
instance TypedHeader (ContentType Type.MIME.Azw) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Azw)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Azw)
instance TypedHeader (ContentType Type.MIME.Bin) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bin)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bin)
instance TypedHeader (ContentType Type.MIME.Bmp) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bmp)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bmp)
instance TypedHeader (ContentType Type.MIME.Bz) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bz)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bz)
instance TypedHeader (ContentType Type.MIME.Bz2) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Bz2)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Bz2)
instance TypedHeader (ContentType Type.MIME.Cda) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Cda)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Cda)
instance TypedHeader (ContentType Type.MIME.Csh) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Csh)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Csh)
instance TypedHeader (ContentType Type.MIME.Css) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Css)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Css)
instance TypedHeader (ContentType Type.MIME.Csv) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Csv)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Csv)
instance TypedHeader (ContentType Type.MIME.Doc) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Doc)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Doc)
instance TypedHeader (ContentType Type.MIME.Docx) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Docx)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Docx)
instance TypedHeader (ContentType Type.MIME.Eot) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Eot)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Eot)
instance TypedHeader (ContentType Type.MIME.Epub) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Epub)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Epub)
instance TypedHeader (ContentType Type.MIME.Gz) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Gz)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Gz)
instance TypedHeader (ContentType Type.MIME.Gif) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Gif)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Gif)
instance TypedHeader (ContentType Type.MIME.Html) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Html)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Html)
instance TypedHeader (ContentType Type.MIME.Ico) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ico)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ico)
instance TypedHeader (ContentType Type.MIME.Ics) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ics)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ics)
instance TypedHeader (ContentType Type.MIME.Jar) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Jar)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Jar)
instance TypedHeader (ContentType Type.MIME.Jpeg) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Jpeg)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Jpeg)
instance TypedHeader (ContentType Type.MIME.Js) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Js)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Js)
instance TypedHeader (ContentType Type.MIME.Json) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Json)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Json)
instance TypedHeader (ContentType Type.MIME.Jsonld) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Jsonld)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Jsonld)
instance TypedHeader (ContentType Type.MIME.Midi) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Midi)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Midi)
instance TypedHeader (ContentType Type.MIME.Mjs) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mjs)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mjs)
instance TypedHeader (ContentType Type.MIME.Mp3) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mp3)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mp3)
instance TypedHeader (ContentType Type.MIME.Mp4) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mp4)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mp4)
instance TypedHeader (ContentType Type.MIME.Mpeg) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mpeg)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mpeg)
instance TypedHeader (ContentType Type.MIME.Mpkg) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Mpkg)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Mpkg)
instance TypedHeader (ContentType Type.MIME.Odp) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Odp)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Odp)
instance TypedHeader (ContentType Type.MIME.Ods) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ods)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ods)
instance TypedHeader (ContentType Type.MIME.Odt) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Odt)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Odt)
instance TypedHeader (ContentType Type.MIME.Oga) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Oga)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Oga)
instance TypedHeader (ContentType Type.MIME.Ogv) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ogv)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ogv)
instance TypedHeader (ContentType Type.MIME.Ogx) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ogx)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ogx)
instance TypedHeader (ContentType Type.MIME.Opus) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Opus)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Opus)
instance TypedHeader (ContentType Type.MIME.Otf) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Otf)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Otf)
instance TypedHeader (ContentType Type.MIME.Png) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Png)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Png)
instance TypedHeader (ContentType Type.MIME.Pdf) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Pdf)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Pdf)
instance TypedHeader (ContentType Type.MIME.Php) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Php)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Php)
instance TypedHeader (ContentType Type.MIME.Ppt) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ppt)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ppt)
instance TypedHeader (ContentType Type.MIME.Pptx) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Pptx)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Pptx)
instance TypedHeader (ContentType Type.MIME.Rar) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Rar)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Rar)
instance TypedHeader (ContentType Type.MIME.Rtf) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Rtf)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Rtf)
instance TypedHeader (ContentType Type.MIME.Sh) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Sh)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Sh)
instance TypedHeader (ContentType Type.MIME.Svg) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Svg)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Svg)
instance TypedHeader (ContentType Type.MIME.Tar) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Tar)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Tar)
instance TypedHeader (ContentType Type.MIME.Tif) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Tif)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Tif)
instance TypedHeader (ContentType Type.MIME.Ts) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ts)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ts)
instance TypedHeader (ContentType Type.MIME.Ttf) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Ttf)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Ttf)
instance TypedHeader (ContentType Type.MIME.Txt) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Txt)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Txt)
instance TypedHeader (ContentType Type.MIME.Vsd) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Vsd)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Vsd)
instance TypedHeader (ContentType Type.MIME.Wav) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Wav)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Wav)
instance TypedHeader (ContentType Type.MIME.Weba) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Weba)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Weba)
instance TypedHeader (ContentType Type.MIME.Webm) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Webm)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Webm)
instance TypedHeader (ContentType Type.MIME.Webp) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Webp)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Webp)
instance TypedHeader (ContentType Type.MIME.Woff) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Woff)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Woff)
instance TypedHeader (ContentType Type.MIME.Woff2) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Woff2)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Woff2)
instance TypedHeader (ContentType Type.MIME.Xhtml) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xhtml)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xhtml)
instance TypedHeader (ContentType Type.MIME.Xls) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xls)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xls)
instance TypedHeader (ContentType Type.MIME.Xlsx) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xlsx)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xlsx)
instance TypedHeader (ContentType Type.MIME.Xml) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xml)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xml)
instance TypedHeader (ContentType Type.MIME.Xul) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Xul)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Xul)
instance TypedHeader (ContentType Type.MIME.Zip) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Zip)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Zip)
instance TypedHeader (ContentType Type.MIME.Video3gp) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Video3gp)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Video3gp)
instance TypedHeader (ContentType Type.MIME.Video3g2) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Video3g2)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Video3g2)
instance TypedHeader (ContentType Type.MIME.Archive7z) where
  headerName = "Content-Type"
  headerValueParser = mimeParser >>= (Parse.liftMaybe (const $ "expected " <> MIME.toString (Type.MIME.value @Type.MIME.Archive7z)) <<< Type.MIME.fromValue) <#> ContentType
  headerValueEncode _ = MIME.toString (Type.MIME.value @Type.MIME.Archive7z)

instance TypedHeader AccessControlAllowCredentials where
  headerName = "Access-Control-Allow-Credentials"
  headerValueParser = Parse.String.string "true" $> AccessControlAllowCredentials
  headerValueEncode _ = "true"

instance TypedHeader AccessControlAllowHeaders where
  headerName = "Access-Control-Allow-Headers"
  headerValueParser =
    let
      headers = commas1 headerNameParser <#> Right <#> AccessControlAllowHeaders
    in
      (wildcardParser $> AccessControlAllowHeaders (Left Wildcard)) <|> headers
  headerValueEncode (AccessControlAllowHeaders (Left Wildcard)) = "*"
  headerValueEncode (AccessControlAllowHeaders (Right hs)) = hs <#> String.Lower.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader AccessControlAllowMethods where
  headerName = "Access-Control-Allow-Methods"
  headerValueParser =
    let
      methods = commas1 methodParser <#> Right <#> AccessControlAllowMethods
    in
      (wildcardParser $> AccessControlAllowMethods (Left Wildcard)) <|> methods
  headerValueEncode (AccessControlAllowMethods (Left Wildcard)) = "*"
  headerValueEncode (AccessControlAllowMethods (Right ms)) = ms <#> Method.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader AccessControlAllowOrigin where
  headerName = "Access-Control-Allow-Origin"
  headerValueParser =
    let
      str = Parse.String.rest <#> Right <#> AccessControlAllowOrigin
    in
      (wildcardParser $> AccessControlAllowOrigin (Left Wildcard)) <|> str
  headerValueEncode (AccessControlAllowOrigin (Left Wildcard)) = "*"
  headerValueEncode (AccessControlAllowOrigin (Right a)) = a

instance TypedHeader AccessControlExposeHeaders where
  headerName = "Access-Control-Expose-Headers"
  headerValueParser =
    let
      str = commas headerNameParser <#> Right <#> AccessControlExposeHeaders
    in
      (wildcardParser $> AccessControlExposeHeaders (Left Wildcard)) <|> str
  headerValueEncode (AccessControlExposeHeaders (Left Wildcard)) = "*"
  headerValueEncode (AccessControlExposeHeaders (Right hs)) = hs <#> String.Lower.toString # Array.intercalate ", "

instance TypedHeader AccessControlMaxAge where
  headerName = "Access-Control-Max-Age"
  headerValueParser = Parse.String.Basic.intDecimal <#> AccessControlMaxAge
  headerValueEncode (AccessControlMaxAge i) = Int.toStringAs Int.decimal i

instance TypedHeader AccessControlRequestHeaders where
  headerName = "Access-Control-Request-Headers"
  headerValueParser = commas1 headerNameParser <#> AccessControlRequestHeaders
  headerValueEncode (AccessControlRequestHeaders hs) = hs <#> String.Lower.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader AccessControlRequestMethod where
  headerName = "Access-Control-Request-Method"
  headerValueParser = methodParser <#> AccessControlRequestMethod
  headerValueEncode (AccessControlRequestMethod m) = Method.toString m

instance TypedHeader Age where
  headerName = "Age"
  headerValueParser = Parse.String.Basic.intDecimal <#> Age
  headerValueEncode (Age i) = Int.toStringAs Int.decimal i

instance TypedHeader Allow where
  headerName = "Allow"
  headerValueParser = commas1 methodParser <#> Allow
  headerValueEncode (Allow ms) = ms <#> Method.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader Authorization where
  headerName = "Authorization"
  headerValueParser =
    let
      scheme = (Parse.String.anyTill (Parse.String.Basic.space) <#> fst <#> AuthScheme)
    in
      pure Authorization <*> scheme <*> (Parse.String.rest <#> String.trim)
  headerValueEncode (Authorization (AuthScheme s) v) = s <> " " <> v

instance TypedHeader BasicAuth where
  headerName = "Authorization"
  headerValueParser = do
    Authorization (AuthScheme s) v <- headerValueParser @Authorization
    when (String.toLower s /= "basic") $ Parse.fail $ "expected Authorization scheme to be Basic, found " <> s
    decoded <- String.Base64.decode v # lmap Error.message # Parse.liftEither
    case String.split (wrap ":") decoded of
      [username, password] -> pure $ BasicAuth {username, password}
      _ -> Parse.fail "malformed Basic Auth"
  headerValueEncode (BasicAuth {username, password}) =
    headerValueEncode $ Authorization (AuthScheme "Basic") (String.Base64.encode (username <> ":" <> password))

instance TypedHeader BearerAuth where
  headerName = "Authorization"
  headerValueParser = do
    Authorization (AuthScheme s) v <- headerValueParser @Authorization
    when (String.toLower s /= "basic") $ Parse.fail $ "expected Authorization scheme to be Bearer, found " <> s
    pure $ BearerAuth v
  headerValueEncode (BearerAuth a) =
    headerValueEncode $ Authorization (AuthScheme "Bearer") a

instance TypedHeader CacheControl where
  headerName = "Cache-Control"
  headerValueParser = do
    directives <- commas1 directiveParser <#> Map.fromFoldable
    pure $ CacheControl
      { maxAge: Map.lookup "max-age" directives # join >>= Int.fromString
  , maxStale: Map.lookup "max-stale" directives # join >>= Int.fromString
  , minFresh: Map.lookup "min-fresh" directives # join >>= Int.fromString
  , sMaxAge: Map.lookup "s-maxage" directives # join >>= Int.fromString
  , noCache: Map.lookup "no-cache" directives # isJust
  , noStore: Map.lookup "no-store" directives # isJust
  , noTransform: Map.lookup "no-transform" directives # isJust
  , onlyIfCached: Map.lookup "only-if-cached" directives # isJust
  , mustRevalidate: Map.lookup "must-revalidate" directives # isJust
  , mustUnderstand: Map.lookup "must-understand" directives # isJust
  , proxyRevalidate: Map.lookup "proxy-revalidate" directives # isJust
  , private: Map.lookup "private" directives # isJust
  , public: Map.lookup "public" directives # isJust
  , immutable: Map.lookup "immutable" directives # isJust
  , staleWhileRevalidate: Map.lookup "stale-while-revalidate" directives # isJust
  , staleIfError: Map.lookup "stale-if-error" directives # isJust
  }
  headerValueEncode (CacheControl a) =
    let
      flag v k
        | v = Just k
        | otherwise = Nothing
      int (Just i) k = Just (k <> "=" <> Int.toStringAs Int.decimal i)
      int Nothing _ = Nothing
    in
      Array.intercalate ", " $ Array.catMaybes
      [ int a.maxAge "max-age"
      , int a.maxStale "max-stale"
      , int a.minFresh "min-fresh"
      , int a.sMaxAge "s-maxage"
      , flag a.noCache "no-cache"
      , flag a.noStore "no-store"
      , flag a.noTransform "no-transform"
      , flag a.onlyIfCached "only-if-cached"
      , flag a.mustRevalidate "must-revalidate"
      , flag a.mustUnderstand "must-understand"
      , flag a.proxyRevalidate "proxy-revalidate"
      , flag a.private "private"
      , flag a.public "public"
      , flag a.immutable "immutable"
      , flag a.staleWhileRevalidate "stale-while-revalidate"
      , flag a.staleIfError "stale-if-error"
      ]
