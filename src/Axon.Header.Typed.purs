module Axon.Header.Typed where

import Prelude

import Axon.Request.Method (Method)
import Axon.Request.Method as Method
import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Array.NonEmpty
import Data.Bifunctor (lmap)
import Data.Date as Date
import Data.Date.Component (Month(..), Weekday(..))
import Data.DateTime (DateTime(..))
import Data.DateTime as DateTime
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Either.Nested as Either.Nested
import Data.Enum (fromEnum, toEnum)
import Data.Filterable (filter)
import Data.Foldable (any, elem)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.MIME as MIME
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.Base64 as String.Base64
import Data.String.CodeUnits as String.CodeUnit
import Data.String.Lower (StringLower)
import Data.String.Lower as String.Lower
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.Time as Time
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Console (log)
import Effect.Exception as Error
import Effect.Unsafe (unsafePerformEffect)
import Parsing (Parser)
import Parsing (liftMaybe, fail, liftEither) as Parse
import Parsing.Combinators (between, choice, lookAhead, many, optional, sepBy, sepBy1, try) as Parse
import Parsing.String (anyTill, regex, string, eof, rest) as Parse
import Parsing.String.Basic (whiteSpace, space, intDecimal, alphaNum) as Parse
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Nub, class Union)
import Record as Record
import Type.MIME as Type.MIME

data Wildcard = Wildcard
derive instance Generic Wildcard _
derive instance Eq Wildcard
instance Show Wildcard where show = genericShow

quotesRe :: Regex
quotesRe = unsafePartial $ (\(Right a) -> a) $ Regex.regex "(^\\s*\")|(\"\\s*$)" Regex.Flags.global

datetimeParser :: Parser String DateTime
datetimeParser =
  let
    as :: forall a. String -> a -> Parser String a
    as s a = Parse.string s $> a

    weekday = Parse.choice [as "Mon" Monday, as "Tue" Tuesday, as "Wed" Wednesday, as "Thu" Thursday, as "Fri" Friday, as "Sat" Saturday, as "Sun" Sunday]
    month = Parse.choice [as "Jan" January, as "Feb" February, as "Mar" March, as "Apr" April, as "May" May, as "Jun" June, as "Jul" July, as "Aug" August, as "Sep" September, as "Oct" October, as "Nov" November, as "Dec" December]
    day = Parse.intDecimal <#> toEnum >>= Parse.liftMaybe (const "invalid day")
    year = Parse.intDecimal <#> toEnum >>= Parse.liftMaybe (const "invalid year")
    date =
      ( pure (\d m y -> Date.exactDate y m d)
          <*> (weekday *> Parse.whiteSpace *> day)
          <*> (Parse.whiteSpace *> month) <*> (Parse.whiteSpace *> year)
      )
      >>= Parse.liftMaybe (const "invalid date")

    time =
      ( pure (\h m s ms -> Time.Time h m s ms)
          <*> ((Parse.intDecimal <#> toEnum >>= Parse.liftMaybe (const "invalid hour")))
          <*> (Parse.string ":" *> (Parse.intDecimal <#> toEnum >>= Parse.liftMaybe (const "invalid minutes")))
          <*> (Parse.string ":" *> (Parse.intDecimal <#> toEnum >>= Parse.liftMaybe (const "invalid seconds")))
          <*> (toEnum 0 # Parse.liftMaybe (const "invalid milliseconds"))
      )
  in
    pure DateTime <*> (date <* Parse.whiteSpace) <*> time


printDateTime :: DateTime -> String
printDateTime dt =
  let
    weekday = case _ of
      Monday -> "Mon"
      Tuesday -> "Tue"
      Wednesday -> "Wed"
      Thursday -> "Thu"
      Friday -> "Fri"
      Saturday -> "Sat"
      Sunday -> "Sun"
    month =
      case _ of
        January -> "Jan"
        February -> "Feb"
        March -> "Mar"
        April -> "Apr"
        May -> "May"
        June -> "Jun"
        July -> "Jul"
        August -> "Aug"
        September -> "Sep"
        October -> "Oct"
        November -> "Nov"
        December -> "Dec"

    time =
        [ dt # DateTime.time # DateTime.hour # fromEnum  # Int.toStringAs Int.decimal
        , dt # DateTime.time # DateTime.minute # fromEnum # Int.toStringAs Int.decimal
        , dt # DateTime.time # DateTime.second # fromEnum # Int.toStringAs Int.decimal
        ]
        # Array.intercalate ":"
    in
        [ weekday (DateTime.weekday $ DateTime.date dt) <> ","
        , dt # DateTime.date # DateTime.day # fromEnum # Int.toStringAs Int.decimal
        , dt # DateTime.date # DateTime.month # month
        , dt # DateTime.date # DateTime.year # fromEnum # Int.toStringAs Int.decimal
        , time
        ]
        # Array.intercalate " "

commas :: forall a. Parser String a -> Parser String (Array a)
commas p = Parse.sepBy p (Parse.whiteSpace <* Parse.string "," <* Parse.whiteSpace) <#> Array.fromFoldable

commas1 :: forall a. Parser String a -> Parser String (NonEmptyArray a)
commas1 p = Parse.sepBy1 p (Parse.whiteSpace <* Parse.string "," <* Parse.whiteSpace) <#> Array.NonEmpty.fromFoldable1

semis :: forall a. Parser String a -> Parser String (Array a)
semis p = Parse.sepBy p (Parse.whiteSpace <* Parse.string ";" <* Parse.whiteSpace) <#> Array.fromFoldable

wildcardParser :: Parser String Wildcard
wildcardParser = Parse.whiteSpace *> (Parse.string "*" $> Wildcard) <* Parse.whiteSpace

mimeParser :: Parser String MIME.MIME
mimeParser = Parse.anyTill (void Parse.space <|> Parse.eof) <#> fst <#> MIME.fromString

headerNameRegexParser :: Parser String String
headerNameRegexParser = unsafePartial $ (\(Right a) -> a) $ Parse.regex "[\\w-]+" Regex.Flags.noFlags

closeRegexParser :: Parser String String
closeRegexParser = unsafePartial $ (\(Right a) -> a) $ Parse.regex "\\s*close\\s*" Regex.Flags.ignoreCase

headerNameParser :: Parser String StringLower
headerNameParser = Parse.between Parse.whiteSpace Parse.whiteSpace (headerNameRegexParser <#> String.Lower.fromString)

methodParser :: Parser String Method
methodParser = Parse.between Parse.whiteSpace Parse.whiteSpace $ Parse.many Parse.alphaNum <#> Array.fromFoldable <#> String.CodeUnit.fromCharArray >>= (\a -> Parse.liftMaybe (const $ "invalid method " <> a) $ Method.fromString a)

directiveParser :: Parser String (StringLower /\ Maybe String)
directiveParser =
  let
    boundary = Parse.lookAhead $ Parse.try (Parse.string ",") <|> Parse.try (Parse.string ";") <|> Parse.try (Parse.eof *> pure "")
    kvSep = Parse.string "="
    kvParser =
      pure (\k v -> k /\ Just v)
        <*> (Parse.whiteSpace *> Parse.anyTill kvSep <#> fst <#> String.trim <#> String.Lower.fromString)
        <*> (Parse.whiteSpace *> Parse.anyTill boundary <#> fst <#> String.trim)
    kParser =
      Parse.whiteSpace
      *> Parse.anyTill boundary
          <#> fst
          <#> String.trim
          <#> String.Lower.fromString
          <#> (\k -> k /\ Nothing)
  in
    (Parse.try kvParser <|> kParser)
      <#> Just
      <#> filter (\(k /\ v) -> not (String.null $ String.Lower.toString k) && maybe true (not <<< String.null) v)
      >>= (Parse.liftMaybe $ const "empty directive")

class TypedHeader a where
  headerName :: String
  headerValueParser :: Parser String a
  headerValueEncode :: a -> String

newtype Accept a = Accept a
derive instance Generic (Accept a) _
derive instance Newtype (Accept a) _
derive instance Eq a => Eq (Accept a)
instance Show a => Show (Accept a) where show = genericShow

data AccessControlAllowCredentials = AccessControlAllowCredentials
derive instance Generic AccessControlAllowCredentials _
derive instance Eq AccessControlAllowCredentials
instance Show AccessControlAllowCredentials where show = genericShow

newtype AccessControlAllowHeaders = AccessControlAllowHeaders (Wildcard \/ NonEmptyArray StringLower)
derive instance Generic (AccessControlAllowHeaders) _
derive instance Newtype (AccessControlAllowHeaders) _
derive instance Eq (AccessControlAllowHeaders)
instance Show AccessControlAllowHeaders where show = genericShow

newtype AccessControlAllowMethods = AccessControlAllowMethods (Wildcard \/ NonEmptyArray Method)
derive instance Newtype (AccessControlAllowMethods) _
derive instance Eq (AccessControlAllowMethods)
derive instance Generic (AccessControlAllowMethods) _
instance Show (AccessControlAllowMethods) where show = genericShow

newtype AccessControlAllowOrigin = AccessControlAllowOrigin (Wildcard \/ String)
derive instance Newtype (AccessControlAllowOrigin) _
derive instance Eq (AccessControlAllowOrigin)
derive instance Generic (AccessControlAllowOrigin) _
instance Show (AccessControlAllowOrigin) where show = genericShow

newtype AccessControlExposeHeaders = AccessControlExposeHeaders (Wildcard \/ Array StringLower)
derive instance Newtype (AccessControlExposeHeaders) _
derive instance Eq (AccessControlExposeHeaders)
derive instance Generic (AccessControlExposeHeaders) _
instance Show (AccessControlExposeHeaders) where show = genericShow

newtype AccessControlMaxAge = AccessControlMaxAge Int
derive instance Newtype (AccessControlMaxAge) _
derive instance Eq (AccessControlMaxAge)
derive instance Generic (AccessControlMaxAge) _
instance Show (AccessControlMaxAge) where show = genericShow

newtype AccessControlRequestHeaders = AccessControlRequestHeaders (NonEmptyArray StringLower)
derive instance Newtype (AccessControlRequestHeaders) _
derive instance Eq (AccessControlRequestHeaders)
derive instance Generic (AccessControlRequestHeaders) _
instance Show (AccessControlRequestHeaders) where show = genericShow

newtype AccessControlRequestMethod = AccessControlRequestMethod Method
derive instance Newtype (AccessControlRequestMethod) _
derive instance Eq (AccessControlRequestMethod)
derive instance Generic (AccessControlRequestMethod) _
instance Show (AccessControlRequestMethod) where show = genericShow

newtype Age = Age Int
derive instance Newtype (Age) _
derive instance Generic (Age) _
derive instance Eq (Age)
instance Show (Age) where show = genericShow

newtype Allow = Allow (NonEmptyArray Method)
derive instance Newtype (Allow) _
derive instance Generic (Allow) _
derive instance Eq (Allow)
instance Show (Allow) where show = genericShow

newtype AuthScheme = AuthScheme String
derive instance Newtype (AuthScheme) _
derive instance Generic (AuthScheme) _
derive instance Eq (AuthScheme)
instance Show (AuthScheme) where show = genericShow

data Authorization = Authorization AuthScheme String
derive instance Generic (Authorization) _
derive instance Eq (Authorization)
instance Show (Authorization) where show = genericShow

newtype BearerAuth = BearerAuth String
derive instance Newtype (BearerAuth) _
derive instance Generic (BearerAuth) _
derive instance Eq (BearerAuth)
instance Show (BearerAuth) where show = genericShow

newtype BasicAuth = BasicAuth {username :: String, password :: String}
derive instance Newtype (BasicAuth) _
derive instance Generic (BasicAuth) _
derive instance Eq (BasicAuth)
instance Show (BasicAuth) where show = genericShow

newtype ByteRangeStart = ByteRangeStart Int
derive instance Newtype (ByteRangeStart) _
derive instance Generic (ByteRangeStart) _
derive instance Eq (ByteRangeStart)
instance Show (ByteRangeStart) where show = genericShow

newtype ByteRangeEnd = ByteRangeEnd Int
derive instance Newtype (ByteRangeEnd) _
derive instance Generic (ByteRangeEnd) _
derive instance Eq (ByteRangeEnd)
instance Show (ByteRangeEnd) where show = genericShow

newtype ByteRangeLength = ByteRangeLength Int
derive instance Newtype (ByteRangeLength) _
derive instance Generic (ByteRangeLength) _
derive instance Eq (ByteRangeLength)
instance Show (ByteRangeLength) where show = genericShow

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
derive instance Generic (CacheControl) _
derive instance Eq (CacheControl)
instance Show (CacheControl) where show = genericShow

data ConnectionClose = ConnectionClose
derive instance Eq (ConnectionClose)
instance Show (ConnectionClose) where show = genericShow
derive instance Generic (ConnectionClose) _

newtype Connection = Connection (ConnectionClose \/ NonEmptyArray StringLower)
derive instance Newtype (Connection) _
derive instance Generic (Connection) _
derive instance Eq (Connection)
instance Show (Connection) where show = genericShow

newtype ContentDisposition = ContentDisposition (ContentDispositionInline \/ ContentDispositionAttachment \/ ContentDispositionFormData \/ Void)
derive instance Newtype (ContentDisposition) _
derive instance Generic (ContentDisposition) _
derive instance Eq (ContentDisposition)
instance Show (ContentDisposition) where show = genericShow

data ContentDispositionInline = ContentDispositionInline
derive instance Generic (ContentDispositionInline) _
derive instance Eq (ContentDispositionInline)
instance Show (ContentDispositionInline) where show = genericShow

newtype ContentDispositionAttachment = ContentDispositionAttachment {filename :: Maybe String}
derive instance Newtype (ContentDispositionAttachment) _
derive instance Generic (ContentDispositionAttachment) _
derive instance Eq (ContentDispositionAttachment)
instance Show (ContentDispositionAttachment) where show = genericShow

newtype ContentDispositionFormData = ContentDispositionFormData {filename :: Maybe String, name :: Maybe String}
derive instance Newtype (ContentDispositionFormData) _
derive instance Generic (ContentDispositionFormData) _
derive instance Eq (ContentDispositionFormData)
instance Show (ContentDispositionFormData) where show = genericShow

newtype ContentEncoding = ContentEncoding (NonEmptyArray String)
derive instance Newtype (ContentEncoding) _
derive instance Generic (ContentEncoding) _
derive instance Eq (ContentEncoding)
instance Show (ContentEncoding) where show = genericShow

newtype ContentLength = ContentLength Int
derive instance Newtype (ContentLength) _
derive instance Generic (ContentLength) _
derive instance Eq (ContentLength)
instance Show (ContentLength) where show = genericShow

newtype ContentLocation = ContentLocation String
derive instance Newtype (ContentLocation) _
derive instance Generic (ContentLocation) _
derive instance Eq (ContentLocation)
instance Show (ContentLocation) where show = genericShow

newtype ContentRange = ContentRange ((ByteRangeStart /\ ByteRangeEnd /\ ByteRangeLength) \/ (ByteRangeStart /\ ByteRangeEnd) \/ ByteRangeLength \/ Void)
derive instance Newtype (ContentRange) _
derive instance Generic (ContentRange) _
derive instance Eq (ContentRange)
instance Show (ContentRange) where show = genericShow

newtype ContentType a = ContentType a
derive instance Generic (ContentType a) _
derive instance Newtype (ContentType a) _
derive instance Eq a => Eq (ContentType a)
instance Show a => Show (ContentType a) where show = genericShow

newtype Cookie = Cookie String
derive instance Newtype (Cookie) _
derive instance Generic (Cookie) _
instance Show (Cookie) where show = genericShow
derive instance Eq (Cookie)

newtype Date = Date DateTime
derive instance Newtype (Date) _
derive instance Generic (Date) _
instance Show (Date) where show = genericShow
derive instance Eq (Date)

newtype ETag = ETag String
derive instance Newtype (ETag) _
derive instance Generic (ETag) _
instance Show (ETag) where show = genericShow
derive instance Eq (ETag)

data ExpectContinue = ExpectContinue

newtype Expires = Expires DateTime
derive instance Newtype (Expires) _
derive instance Generic (Expires) _
instance Show (Expires) where show = genericShow
derive instance Eq (Expires)

newtype Host = Host String
derive instance Newtype (Host) _
derive instance Generic (Host) _
instance Show (Host) where show = genericShow
derive instance Eq (Host)

newtype IfMatch = IfMatch (Wildcard \/ NonEmptyArray String)
derive instance Newtype (IfMatch) _
derive instance Generic (IfMatch) _
instance Show (IfMatch) where show = genericShow
derive instance Eq (IfMatch)

newtype IfNoneMatch = IfNoneMatch (Wildcard \/ NonEmptyArray MatchETag)
derive instance Newtype (IfNoneMatch) _
derive instance Generic (IfNoneMatch) _
instance Show (IfNoneMatch) where show = genericShow
derive instance Eq (IfNoneMatch)

newtype IfModifiedSince = IfModifiedSince DateTime
derive instance Newtype (IfModifiedSince) _
derive instance Generic (IfModifiedSince) _
instance Show (IfModifiedSince) where show = genericShow
derive instance Eq (IfModifiedSince)

newtype IfRange = IfRange (DateTime \/ String)
derive instance Newtype (IfRange) _
derive instance Generic (IfRange) _
instance Show (IfRange) where show = genericShow
derive instance Eq (IfRange)

newtype IfUnmodifiedSince = IfUnmodifiedSince DateTime
derive instance Newtype (IfUnmodifiedSince) _
derive instance Generic (IfUnmodifiedSince) _
instance Show (IfUnmodifiedSince) where show = genericShow
derive instance Eq (IfUnmodifiedSince)

newtype LastModified = LastModified DateTime
derive instance Newtype (LastModified) _
derive instance Generic (LastModified) _
instance Show (LastModified) where show = genericShow
derive instance Eq (LastModified)

data MatchETag = MatchETag String | MatchETagWeak String
derive instance Eq MatchETag 
instance Show MatchETag where show = genericShow
derive instance Generic MatchETag _

newtype Origin = Origin String
derive instance Newtype (Origin) _
derive instance Generic (Origin) _
instance Show (Origin) where show = genericShow
derive instance Eq (Origin)

data ProxyAuthorization = ProxyAuthorization AuthScheme String
derive instance Generic (ProxyAuthorization) _
derive instance Eq (ProxyAuthorization)
instance Show (ProxyAuthorization) where show = genericShow

type RangeSpecifier = ByteRangeStart \/ (ByteRangeStart /\ ByteRangeEnd) \/ ByteRangeLength \/ Void

newtype Range = Range (NonEmptyArray RangeSpecifier)
derive instance Newtype (Range) _
derive instance Generic (Range) _
instance Show (Range) where show = genericShow
derive instance Eq (Range)

newtype Referer = Referer String
derive instance Newtype (Referer) _
derive instance Generic (Referer) _
instance Show (Referer) where show = genericShow
derive instance Eq (Referer)

data ReferrerPolicy
  = ReferrerPolicyNoReferrer
  | ReferrerPolicyNoReferrerWhenDowngrade
  | ReferrerPolicyOrigin
  | ReferrerPolicyOriginWhenCrossOrigin
  | ReferrerPolicySameOrigin
  | ReferrerPolicyStrictOrigin
  | ReferrerPolicyStrictOriginWhenCrossOrigin
  | ReferrerPolicyUnsafeURL

derive instance Generic (ReferrerPolicy) _
derive instance Eq (ReferrerPolicy)
instance Show (ReferrerPolicy) where show = genericShow

newtype RetryAfter = RetryAfter (DateTime \/ Int)
derive instance Newtype (RetryAfter) _
derive instance Generic (RetryAfter) _
instance Show (RetryAfter) where show = genericShow
derive instance Eq (RetryAfter)

newtype SecWebsocketKey = SecWebsocketKey String
derive instance Newtype (SecWebsocketKey) _
derive instance Generic (SecWebsocketKey) _
instance Show (SecWebsocketKey) where show = genericShow
derive instance Eq (SecWebsocketKey)

newtype SecWebsocketAccept = SecWebsocketAccept SecWebsocketKey
derive instance Newtype (SecWebsocketAccept) _
derive instance Generic (SecWebsocketAccept) _
instance Show (SecWebsocketAccept) where show = genericShow
derive instance Eq (SecWebsocketAccept)

newtype SecWebsocketVersion = SecWebsocketVersion (NonEmptyArray Int)
derive instance Newtype (SecWebsocketVersion) _
derive instance Generic (SecWebsocketVersion) _
instance Show (SecWebsocketVersion) where show = genericShow
derive instance Eq (SecWebsocketVersion)

newtype Server = Server String
derive instance Newtype (Server) _
derive instance Generic (Server) _
instance Show (Server) where show = genericShow
derive instance Eq (Server)

newtype SetCookie = SetCookie String
derive instance Newtype (SetCookie) _
derive instance Generic (SetCookie) _
instance Show (SetCookie) where show = genericShow
derive instance Eq (SetCookie)

newtype StrictTransportSecurity = StrictTransportSecurity {maxAge :: Maybe Int, includeSubdomains :: Boolean}
derive instance Newtype (StrictTransportSecurity) _
derive instance Generic (StrictTransportSecurity) _
instance Show (StrictTransportSecurity) where show = genericShow
derive instance Eq (StrictTransportSecurity)

newtype TE = TE String
derive instance Newtype (TE) _
derive instance Generic (TE) _
instance Show (TE) where show = genericShow
derive instance Eq (TE)

newtype TransferEncoding = TransferEncoding String
derive instance Newtype (TransferEncoding) _
derive instance Generic (TransferEncoding) _
instance Show (TransferEncoding) where show = genericShow
derive instance Eq (TransferEncoding)

newtype Upgrade = Upgrade (NonEmptyArray String)
derive instance Newtype (Upgrade) _
derive instance Generic (Upgrade) _
instance Show (Upgrade) where show = genericShow
derive instance Eq (Upgrade)

newtype UserAgent = UserAgent String
derive instance Newtype (UserAgent) _
derive instance Generic (UserAgent) _
instance Show (UserAgent) where show = genericShow
derive instance Eq (UserAgent)

newtype Vary = Vary (Wildcard \/ NonEmptyArray StringLower)
derive instance Newtype (Vary) _
derive instance Generic (Vary) _
instance Show (Vary) where show = genericShow
derive instance Eq (Vary)

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
  headerValueParser = Parse.rest <#> Accept
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
  headerValueParser = Parse.rest <#> ContentType
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
  headerValueParser = Parse.string "true" $> AccessControlAllowCredentials
  headerValueEncode _ = "true"

instance TypedHeader AccessControlAllowHeaders where
  headerName = "Access-Control-Allow-Headers"
  headerValueParser =
    let
      headers = commas1 headerNameParser <#> Right <#> AccessControlAllowHeaders
    in
      Parse.try (wildcardParser $> AccessControlAllowHeaders (Left Wildcard)) <|> Parse.try headers
  headerValueEncode (AccessControlAllowHeaders (Left Wildcard)) = "*"
  headerValueEncode (AccessControlAllowHeaders (Right hs)) = hs <#> String.Lower.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader AccessControlAllowMethods where
  headerName = "Access-Control-Allow-Methods"
  headerValueParser =
    let
      methods = commas1 methodParser <#> Right <#> AccessControlAllowMethods
    in
      Parse.try (wildcardParser $> AccessControlAllowMethods (Left Wildcard)) <|> Parse.try methods
  headerValueEncode (AccessControlAllowMethods (Left Wildcard)) = "*"
  headerValueEncode (AccessControlAllowMethods (Right ms)) = ms <#> Method.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader AccessControlAllowOrigin where
  headerName = "Access-Control-Allow-Origin"
  headerValueParser =
    let
      str = Parse.rest <#> String.trim <#> Right <#> AccessControlAllowOrigin
    in
      Parse.try (wildcardParser $> AccessControlAllowOrigin (Left Wildcard)) <|> Parse.try str
  headerValueEncode (AccessControlAllowOrigin (Left Wildcard)) = "*"
  headerValueEncode (AccessControlAllowOrigin (Right a)) = a

instance TypedHeader AccessControlExposeHeaders where
  headerName = "Access-Control-Expose-Headers"
  headerValueParser =
    let
      str = commas headerNameParser <#> Right <#> AccessControlExposeHeaders
    in
      Parse.try (wildcardParser $> AccessControlExposeHeaders (Left Wildcard)) <|> Parse.try str
  headerValueEncode (AccessControlExposeHeaders (Left Wildcard)) = "*"
  headerValueEncode (AccessControlExposeHeaders (Right hs)) = hs <#> String.Lower.toString # Array.intercalate ", "

instance TypedHeader AccessControlMaxAge where
  headerName = "Access-Control-Max-Age"
  headerValueParser = Parse.between Parse.whiteSpace Parse.whiteSpace Parse.intDecimal <#> AccessControlMaxAge
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
  headerValueParser = Parse.between Parse.whiteSpace Parse.whiteSpace Parse.intDecimal <#> Age
  headerValueEncode (Age i) = Int.toStringAs Int.decimal i

instance TypedHeader Allow where
  headerName = "Allow"
  headerValueParser = commas1 methodParser <#> Allow
  headerValueEncode (Allow ms) = ms <#> Method.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader Authorization where
  headerName = "Authorization"
  headerValueParser =
    let
      scheme = Parse.whiteSpace *> (Parse.anyTill (void (Parse.try Parse.space) <|> Parse.eof) <#> fst <#> String.trim <#> AuthScheme)
    in
      pure Authorization <*> scheme <*> (Parse.rest <#> String.trim)
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
    when (String.toLower s /= "bearer") $ Parse.fail $ "expected Authorization scheme to be Bearer, found " <> s
    pure $ BearerAuth v
  headerValueEncode (BearerAuth a) =
    headerValueEncode $ Authorization (AuthScheme "Bearer") a

instance TypedHeader CacheControl where
  headerName = "Cache-Control"
  headerValueParser = do
    directives <- commas directiveParser <#> map (\(k /\ v) -> String.Lower.toString k /\ v) <#> Map.fromFoldable
    let
      keys =
        [ "max-age"
        , "max-stale"
        , "min-fresh"
        , "s-maxage"
        , "no-cache"
        , "no-store"
        , "no-transform"
        , "only-if-cached"
        , "must-revalidate"
        , "must-understand"
        , "proxy-revalidate"
        , "private"
        , "public"
        , "immutable"
        , "stale-while-revalidate"
        , "stale-if-error"
        ]
    when
      (Map.keys directives # (Set.toUnfoldable :: _ (Array _)) # any (flip elem keys) # not)
      (Parse.fail "no directives")
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

instance TypedHeader Connection where
  headerName = "Connection"
  headerValueParser =
    let
      close = closeRegexParser $> Connection (Left ConnectionClose)
    in
      Parse.try close <|> (commas1 headerNameParser <#> Right <#> Connection)
  headerValueEncode (Connection (Left ConnectionClose)) = "close"
  headerValueEncode (Connection (Right as)) = as <#> String.Lower.toString # Array.NonEmpty.intercalate ", "

instance TypedHeader ContentDisposition where
  headerName = "Content-Disposition"
  headerValueParser =
    let
      boundary = Parse.whiteSpace *> (Parse.try (Parse.string ";") <|> Parse.try (Parse.eof *> pure ""))
      inline = Parse.whiteSpace *> Parse.string "inline" *> boundary $> ContentDisposition (Either.Nested.in1 ContentDispositionInline)
      attachment = do
        void $ Parse.whiteSpace *> Parse.string "attachment" *> boundary *> Parse.whiteSpace
        directives <- semis directiveParser <#> map (\(k /\ v) -> String.Lower.toString k /\ v) <#> Map.fromFoldable
        let
          filename =
            (Map.lookup "filename" directives <|> Map.lookup "filename*" directives)
            # join
            <#> Regex.replace quotesRe ""
        pure $ ContentDisposition $ Either.Nested.in2 $ ContentDispositionAttachment {filename}
      formData = do
        void $ Parse.whiteSpace *> Parse.string "form-data" *> boundary *> Parse.whiteSpace
        directives <- semis directiveParser <#> map (\(k /\ v) -> String.Lower.toString k /\ v) <#> Map.fromFoldable
        let
          filename = Map.lookup "filename" directives # join <#> Regex.replace quotesRe ""
          name = Map.lookup "name" directives # join <#> Regex.replace quotesRe ""
        pure $ ContentDisposition $ Either.Nested.in3 $ ContentDispositionFormData {filename, name}
    in
      Parse.try inline <|> Parse.try attachment <|> Parse.try formData
  headerValueEncode (ContentDisposition a) =
    Either.Nested.either3
      (const $ [Just "inline"])
      (\(ContentDispositionAttachment {filename}) -> [Just "attachment", (\s -> "filename=\"" <> s <> "\"") <$> filename])
      (\(ContentDispositionFormData {filename, name}) -> [Just "attachment", (\s -> "filename=\"" <> s <> "\"") <$> filename, (\s -> "name=\"" <> s <> "\"") <$> name])
      a
    # Array.catMaybes
    # Array.intercalate "; "

instance TypedHeader ContentEncoding where
  headerName = "Content-Encoding"
  headerValueParser =
    commas1 (Parse.whiteSpace *> (Parse.many Parse.alphaNum <#> Array.fromFoldable <#> String.CodeUnit.fromCharArray) <* Parse.whiteSpace)
      <#> ContentEncoding
  headerValueEncode (ContentEncoding as) = Array.NonEmpty.intercalate ", " as

instance TypedHeader ContentLength where
  headerName = "Content-Length"
  headerValueParser = Parse.whiteSpace *> (Parse.intDecimal <#> ContentLength) <* Parse.whiteSpace
  headerValueEncode (ContentLength a) = Int.toStringAs Int.decimal a

instance TypedHeader ContentLocation where
  headerName = "Content-Location"
  headerValueParser = Parse.rest <#> ContentLocation
  headerValueEncode (ContentLocation a) = a

instance TypedHeader ContentRange where
  headerName = "Content-Range"
  headerValueParser =
    let
      startEndSize =
        pure (\a b c -> a /\ b /\ c)
        <*> ((Parse.intDecimal <#> ByteRangeStart) <* Parse.string "-")
        <*> ((Parse.intDecimal <#> ByteRangeEnd) <* Parse.string "/")
        <*> (Parse.intDecimal <#> ByteRangeLength)
      startEnd =
        pure (\a b -> a /\ b)
        <*> ((Parse.intDecimal <#> ByteRangeStart) <* Parse.string "-")
        <*> ((Parse.intDecimal <#> ByteRangeEnd) <* Parse.string "/" <* wildcardParser)
      size =
        wildcardParser
        *> Parse.string "/"
        *> Parse.intDecimal <#> ByteRangeLength
    in
      Parse.string "bytes"
      *> Parse.whiteSpace
      *> Parse.try (startEndSize <#> Either.Nested.in1) <|> Parse.try (startEnd <#> Either.Nested.in2) <|> Parse.try (size <#> Either.Nested.in3)
      <#> ContentRange
  headerValueEncode (ContentRange a) =
    Either.Nested.either3
      (\(ByteRangeStart start /\ ByteRangeEnd end /\ ByteRangeLength len) -> ["bytes ", Int.toStringAs Int.decimal start, "-", Int.toStringAs Int.decimal end, "/", Int.toStringAs Int.decimal len])
      (\(ByteRangeStart start /\ ByteRangeEnd end) -> ["bytes ", Int.toStringAs Int.decimal start, "-", Int.toStringAs Int.decimal end, "/*"])
      (\(ByteRangeLength len) -> ["bytes ", "*/", Int.toStringAs Int.decimal len])
      a
    # Array.fold

instance TypedHeader Cookie where
  headerName = "Cookie"
  headerValueParser = Parse.rest <#> Cookie
  headerValueEncode (Cookie a) = a

instance TypedHeader Date where
  headerName = "Date"
  headerValueParser = datetimeParser <#> Date
  headerValueEncode (Date a) = printDateTime a

instance TypedHeader ETag where
  headerName = "ETag"
  headerValueParser = Parse.rest <#> ETag
  headerValueEncode (ETag a) = a

instance TypedHeader ExpectContinue where
  headerName = "Expect"
  headerValueParser = Parse.string "100-continue" $> ExpectContinue
  headerValueEncode ExpectContinue = "100-continue"

instance TypedHeader Expires where
  headerName = "Expires"
  headerValueParser = datetimeParser <#> Expires
  headerValueEncode (Expires a) = printDateTime a

instance TypedHeader Host where
  headerName = "Host"
  headerValueParser = Parse.rest <#> Host
  headerValueEncode (Host a) = a

instance TypedHeader IfMatch where
  headerName = "If-Match"
  headerValueParser =
    let
      wild = wildcardParser <#> Left <#> IfMatch
      etag =
        Parse.whiteSpace
        *> Parse.optional (Parse.string "W/")
        *> Parse.string "\""
        *> (Parse.anyTill (void $ Parse.string "\"") <#> fst)
        <* Parse.whiteSpace
      etags = commas1 etag <#> Right <#> IfMatch
    in
      Parse.try wild <|> Parse.try etags
  headerValueEncode (IfMatch (Left Wildcard)) = "*"
  headerValueEncode (IfMatch (Right as)) = as <#> (\a -> "\"" <> a <> "\"") # Array.NonEmpty.intercalate ", "

instance TypedHeader IfNoneMatch where
  headerName = "If-None-Match"
  headerValueParser =
    let
      wild = wildcardParser <#> Left <#> IfNoneMatch
      etag a =
        Parse.string "\""
        *> (Parse.anyTill (void $ Parse.string "\"") <#> fst <#> a)
      weakEtag =
        Parse.whiteSpace
        *> Parse.string "W/"
        *> etag MatchETagWeak
        <* Parse.whiteSpace
      strongEtag =
        Parse.whiteSpace
        *> Parse.string "W/"
        *> etag MatchETag
        <* Parse.whiteSpace
      etags = commas1 (Parse.try weakEtag <|> strongEtag) <#> Right <#> IfNoneMatch
    in
      Parse.try wild <|> Parse.try etags
  headerValueEncode (IfNoneMatch (Left Wildcard)) = "*"
  headerValueEncode (IfNoneMatch (Right as)) =
    as
    <#> ( case _ of
            MatchETag a -> "\"" <> a <> "\""
            MatchETagWeak a -> "W/\"" <> a <> "\""
        )
    # Array.NonEmpty.intercalate ", "

instance TypedHeader IfModifiedSince where
  headerName = "If-Modified-Since"
  headerValueParser = datetimeParser <#> IfModifiedSince
  headerValueEncode (IfModifiedSince a) = printDateTime a

instance TypedHeader IfRange where
  headerName = "If-Modified-Since"
  headerValueParser =
    let
      dt = datetimeParser <#> Left <#> IfRange
      etag =
        Parse.whiteSpace
        *> Parse.string "\""
        *> Parse.anyTill (Parse.string "\"")
        <* Parse.whiteSpace
        <#> fst
        <#> Right
        <#> IfRange
    in
      Parse.try dt <|> Parse.try etag
  headerValueEncode (IfRange (Left a)) = printDateTime a
  headerValueEncode (IfRange (Right a)) = "\"" <> a <> "\""

instance TypedHeader IfUnmodifiedSince where
  headerName = "If-Unmodified-Since"
  headerValueParser = datetimeParser <#> IfUnmodifiedSince
  headerValueEncode (IfUnmodifiedSince a) = printDateTime a

instance TypedHeader LastModified where
  headerName = "Last-Modified"
  headerValueParser = datetimeParser <#> LastModified 
  headerValueEncode (LastModified a) = printDateTime a

instance TypedHeader Origin where
  headerName = "Origin"
  headerValueParser = Parse.rest <#> Origin
  headerValueEncode (Origin a) = a

instance TypedHeader ProxyAuthorization where
  headerName = "Proxy-Authorization"
  headerValueParser =
    let
      scheme = (Parse.anyTill (void (Parse.try Parse.space) <|> Parse.eof) <#> fst <#> AuthScheme)
    in
      pure ProxyAuthorization <*> scheme <*> (Parse.rest <#> String.trim)
  headerValueEncode (ProxyAuthorization (AuthScheme s) v) = s <> " " <> v

instance TypedHeader Range where
  headerName = "Range"
  headerValueParser =
    let
      int = Parse.intDecimal
      dash = Parse.string "-"
      rangeSpecStart = int <* dash <#> ByteRangeStart
      rangeSpecStartEnd = pure (\a b -> a /\ b) <*> (int <* dash <#> ByteRangeStart) <*> (int <#> ByteRangeEnd)
      rangeSpecSuffix = dash *> int <#> ByteRangeLength
      rangeSpec =
        Parse.try (rangeSpecStartEnd <#> Either.Nested.in2)
        <|> Parse.try (rangeSpecSuffix <#> Either.Nested.in3)
        <|> (rangeSpecStart <#> Either.Nested.in1)
    in
      commas1 rangeSpec <#> Range
  headerValueEncode (Range as) =
    as
    <#> (
      Either.Nested.either3
        (\(ByteRangeStart a) -> Int.toStringAs Int.decimal a <> "-")
        (\(ByteRangeStart a /\ ByteRangeEnd b) -> Int.toStringAs Int.decimal a <> "-" <> Int.toStringAs Int.decimal b)
        (\(ByteRangeLength b) -> "-" <> Int.toStringAs Int.decimal b)
    )
    # Array.NonEmpty.intercalate ", "

instance TypedHeader Referer where
  headerName = "Referer"
  headerValueParser = Parse.rest <#> Referer
  headerValueEncode (Referer a) = a

instance TypedHeader ReferrerPolicy where
  headerName = "Referrer-Policy"
  headerValueParser =
    let
      noReferrer = Parse.string "no-referrer" $> ReferrerPolicyNoReferrer
      noReferrerWhenDowngrade = Parse.string "no-referrer-when-downgrade" $> ReferrerPolicyNoReferrerWhenDowngrade
      origin = Parse.string "origin" $> ReferrerPolicyOrigin
      originWhenCrossOrigin = Parse.string "origin-when-cross-origin" $> ReferrerPolicyOriginWhenCrossOrigin
      sameOrigin = Parse.string "same-origin" $> ReferrerPolicySameOrigin
      strictOrigin = Parse.string "strict-origin" $> ReferrerPolicyStrictOrigin
      strictOriginWhenCrossOrigin = Parse.string "strict-origin-when-cross-origin" $> ReferrerPolicyStrictOriginWhenCrossOrigin
      unsafeURL = Parse.string "unsafe-url" $> ReferrerPolicyUnsafeURL
    in
      Parse.try noReferrer
      <|> Parse.try noReferrerWhenDowngrade
      <|> Parse.try origin
      <|> Parse.try originWhenCrossOrigin
      <|> Parse.try sameOrigin
      <|> Parse.try strictOrigin
      <|> Parse.try strictOriginWhenCrossOrigin
      <|> unsafeURL
  headerValueEncode ReferrerPolicyNoReferrer = "no-referrer"
  headerValueEncode ReferrerPolicyNoReferrerWhenDowngrade = "no-referrer-when-downgrade"
  headerValueEncode ReferrerPolicyOrigin = "origin"
  headerValueEncode ReferrerPolicyOriginWhenCrossOrigin = "origin-when-cross-origin"
  headerValueEncode ReferrerPolicySameOrigin = "same-origin"
  headerValueEncode ReferrerPolicyStrictOrigin = "strict-origin"
  headerValueEncode ReferrerPolicyStrictOriginWhenCrossOrigin = "strict-origin-when-cross-origin"
  headerValueEncode ReferrerPolicyUnsafeURL = "unsafe-url"

instance TypedHeader RetryAfter where
  headerName = "Retry-After"
  headerValueParser = Parse.try (datetimeParser <#> Left <#> RetryAfter) <|> Parse.try (Parse.intDecimal <#> Right <#> RetryAfter)
  headerValueEncode (RetryAfter (Left a)) = printDateTime a
  headerValueEncode (RetryAfter (Right a)) = Int.toStringAs Int.decimal a

instance TypedHeader SecWebsocketKey where
  headerName = "Sec-WebSocket-Key"
  headerValueParser = Parse.rest <#> SecWebsocketKey
  headerValueEncode (SecWebsocketKey a) = a

instance TypedHeader SecWebsocketAccept where
  headerName = "Sec-WebSocket-Accept"
  headerValueParser = headerValueParser <#> SecWebsocketAccept
  headerValueEncode (SecWebsocketAccept a) = headerValueEncode a

instance TypedHeader SecWebsocketVersion where
  headerName = "Sec-WebSocket-Version"
  headerValueParser = commas1 (Parse.intDecimal) <#> SecWebsocketVersion
  headerValueEncode (SecWebsocketVersion as) = as <#> Int.toStringAs Int.decimal # Array.NonEmpty.intercalate ", "

instance TypedHeader Server where
  headerName = "Server"
  headerValueParser = Parse.rest <#> Server
  headerValueEncode (Server a) = a

instance TypedHeader SetCookie where
  headerName = "Set-Cookie"
  headerValueParser = Parse.rest <#> SetCookie
  headerValueEncode (SetCookie a) = a

instance TypedHeader StrictTransportSecurity where
  headerName = "Strict-Transport-Security"
  headerValueParser = do
    directives <- commas1 directiveParser <#> map (\(k /\ v) -> String.Lower.toString k /\ v) <#> Map.fromFoldable
    pure $ StrictTransportSecurity
      { maxAge: Map.lookup "max-age" directives # join >>= Int.fromString
      , includeSubdomains: Map.lookup "includesubdomains" directives # isJust
      }
  headerValueEncode (StrictTransportSecurity a) =
    let
      flag v k
        | v = Just k
        | otherwise = Nothing
      int (Just i) k = Just (k <> "=" <> Int.toStringAs Int.decimal i)
      int Nothing _ = Nothing
    in
      Array.intercalate ", " $ Array.catMaybes
      [ int a.maxAge "max-age"
      , flag a.includeSubdomains "includeSubDomains"
      ]

instance TypedHeader TE where
  headerName = "TE"
  headerValueParser = Parse.rest <#> TE
  headerValueEncode (TE a) = a

instance TypedHeader TransferEncoding where
  headerName = "Transfer-Encoding"
  headerValueParser = Parse.rest <#> TransferEncoding
  headerValueEncode (TransferEncoding a) = a

instance TypedHeader Upgrade where
  headerName = "Upgrade"
  headerValueParser = commas1 (Parse.anyTill Parse.space <#> fst) <#> Upgrade
  headerValueEncode (Upgrade as) = as # Array.NonEmpty.intercalate ", "

instance TypedHeader UserAgent where
  headerName = "User-Agent"
  headerValueParser = Parse.rest <#> UserAgent
  headerValueEncode (UserAgent a) = a

instance TypedHeader Vary where
  headerName = "Vary"
  headerValueParser = Parse.try (wildcardParser <#> Left <#> Vary) <|> Parse.try (commas1 headerNameParser <#> Right <#> Vary)
  headerValueEncode (Vary (Left _)) = "*"
  headerValueEncode (Vary (Right a)) = a <#> String.Lower.toString # Array.NonEmpty.intercalate ", "
