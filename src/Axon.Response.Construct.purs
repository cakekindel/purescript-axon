module Axon.Response.Construct where

import Prelude

import Axon.Header.Typed (class TypedHeader, headerName, headerValueEncode)
import Axon.Header.Typed as Header
import Axon.Response (Response)
import Axon.Response as Response
import Axon.Response.Body as Response.Body
import Axon.Response.Status (Status)
import Axon.Response.Status as Status
import Data.Argonaut.Core (stringifyWithIndent)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.MIME as MIME
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Node.Buffer (Buffer)
import Node.Stream as Stream
import Prim.Row (class Cons)
import Type.MIME as Type.MIME
import Unsafe.Coerce (unsafeCoerce)

newtype Json a = Json a

derive instance Newtype (Json a) _
derive instance Generic (Json a) _
derive newtype instance (EncodeJson a) => EncodeJson (Json a)
derive newtype instance (DecodeJson a) => DecodeJson (Json a)

class ToResponse a where
  toResponse :: a -> Effect Response

instance ToResponse Response where
  toResponse = pure

else instance ToResponse Status where
  toResponse = pure <<< Response.fromStatus

else instance ToResponse Response.Body where
  toResponse = pure <<< flip (Response.response Status.ok) Map.empty

else instance ToResponse Buffer where
  toResponse = toResponse <<< Response.BodyBuffer

else instance (Cons "read" Stream.Read a' a) => ToResponse (Stream.Stream a) where
  toResponse =
    let
      why :: Stream.Stream a -> Stream.Readable ()
      why = unsafeCoerce
    in
      toResponse <<< Response.Body.stream <<< why

else instance ToResponse String where
  toResponse = toResponse <<< Response.BodyString

else instance EncodeJson a => ToResponse (Json a) where
  toResponse a =
    let
      body = Response.BodyString $ stringifyWithIndent 2 $ encodeJson a
      contentType = (Header.ContentType Type.MIME.Json)
    in
      toResponse body <> toResponse contentType

else instance (ToResponse a) => ToResponse (Status /\ a) where
  toResponse (status /\ a) = toResponse a <#> Response.withStatus status

else instance (ToResponse a, ToResponse b) => ToResponse (a /\ b) where
  toResponse (a /\ b) = pure (<>) <*> toResponse a <*> toResponse b

else instance ToResponse (Header.Accept String) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept MIME.MIME) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Aac) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Abw) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Arc) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Avif) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Avi) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Azw) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Bin) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Bmp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Bz) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Bz2) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Cda) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Csh) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Css) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Csv) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Doc) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Docx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Eot) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Epub) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Gz) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Gif) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Html) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ico) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ics) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Jar) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Jpeg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Js) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Json) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Jsonld) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Midi) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Mp3) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Mp4) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Mpeg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Mpkg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Odp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ods) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Odt) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Oga) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ogv) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ogx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Opus) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Otf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Png) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Pdf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Php) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ppt) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Pptx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Rar) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Rtf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Sh) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Svg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Tar) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Tif) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ts) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Ttf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Txt) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Vsd) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Wav) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Weba) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Webm) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Webp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Woff) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Woff2) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Xhtml) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Xls) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Xlsx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Xml) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Xul) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Zip) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Video3gp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Video3g2) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.Accept Type.MIME.Archive7z) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType String) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType MIME.MIME) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Aac) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Abw) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Arc) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Avif) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Avi) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Azw) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Bin) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Bmp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Bz) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Bz2) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Cda) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Csh) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Css) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Csv) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Doc) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Docx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Eot) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Epub) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Gz) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Gif) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Html) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ico) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ics) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Jar) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Jpeg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Js) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Json) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Jsonld) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Midi) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Mp3) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Mp4) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Mpeg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Mpkg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Odp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ods) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Odt) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Oga) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ogv) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ogx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Opus) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Otf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Png) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Pdf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Php) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ppt) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Pptx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Rar) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Rtf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Sh) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Svg) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Tar) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Tif) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ts) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Ttf) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Txt) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Vsd) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Wav) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Weba) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Webm) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Webp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Woff) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Woff2) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Xhtml) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Xls) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Xlsx) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Xml) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Xul) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Zip) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Video3gp) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Video3g2) where
  toResponse = typedHeaderToResponse

else instance ToResponse (Header.ContentType Type.MIME.Archive7z) where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlAllowCredentials where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlAllowHeaders where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlAllowMethods where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlAllowOrigin where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlExposeHeaders where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlMaxAge where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlRequestHeaders where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.AccessControlRequestMethod where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Age where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Allow where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Authorization where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.BasicAuth where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.BearerAuth where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.CacheControl where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Connection where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ContentDisposition where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ContentEncoding where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ContentLength where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ContentLocation where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ContentRange where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Cookie where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Date where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ETag where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ExpectContinue where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Expires where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Host where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.IfMatch where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.IfNoneMatch where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.IfModifiedSince where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.IfRange where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.IfUnmodifiedSince where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.LastModified where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Origin where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ProxyAuthorization where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Range where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Referer where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.ReferrerPolicy where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.RetryAfter where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.SecWebsocketKey where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.SecWebsocketAccept where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.SecWebsocketVersion where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Server where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.SetCookie where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.StrictTransportSecurity where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.TE where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.TransferEncoding where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Upgrade where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.UserAgent where
  toResponse = typedHeaderToResponse

else instance ToResponse Header.Vary where
  toResponse = typedHeaderToResponse

typedHeaderToResponse :: forall a. TypedHeader a => a -> Effect Response
typedHeaderToResponse =
  pure
    <<< Response.response Status.ok Response.BodyEmpty
    <<< Map.singleton (headerName @a)
    <<< headerValueEncode
