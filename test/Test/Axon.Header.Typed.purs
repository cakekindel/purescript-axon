module Test.Axon.Header.Typed where

import Prelude

import Axon.Header.Typed (class TypedHeader, Accept(..), AccessControlAllowCredentials(..), AccessControlAllowHeaders(..), AccessControlAllowMethods(..), AccessControlAllowOrigin(..), AccessControlExposeHeaders(..), AccessControlMaxAge(..), AccessControlRequestHeaders(..), AccessControlRequestMethod(..), Age(..), Allow(..), AuthScheme(..), Authorization(..), BasicAuth(..), BearerAuth(..), ByteRangeEnd(..), ByteRangeLength(..), ByteRangeStart(..), CacheControl(..), Connection(..), ConnectionClose(..), ContentDisposition(..), ContentDispositionAttachment(..), ContentDispositionFormData(..), ContentDispositionInline(..), ContentEncoding(..), ContentLength(..), ContentLocation(..), ContentRange(..), ContentType(..), Cookie(..), Wildcard(..), cacheControlDefaults, headerValueParser)
import Axon.Request.Method (Method(..))
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isLeft)
import Data.Either.Nested as Either.Nested
import Data.MIME as MIME
import Data.Maybe (Maybe(..))
import Data.String.Lower as String.Lower
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)
import Parsing (parseErrorMessage, runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Type.MIME as Type.MIME

is :: forall h. Eq h => Show h => TypedHeader h => String -> h -> Spec Unit
is i exp = it ("parses " <> show i) do
  act <- runParser i (headerValueParser @h) # lmap (error <<< parseErrorMessage) # liftEither
  act `shouldEqual` exp

isnt :: forall h. Eq h => Show h => TypedHeader h => String -> h -> Spec Unit
isnt i exp = it ("does not parse " <> show i) $
  case runParser i (headerValueParser @h) of
    Left _ -> pure unit
    Right act -> exp `shouldNotEqual` act

spec :: Spec Unit
spec =
  describe "Typed" do
    describe "Accept String" do
      "foo" `is` (Accept "foo")
      "" `is` (Accept "")
    describe "Accept MIME.MIME" do
      "application/json" `is` (Accept MIME.Json)
      "text/plain" `is` (Accept MIME.Txt)
      "text/plain;charset=utf8" `is` (Accept $ MIME.Other "text/plain;charset=utf8")
      "foo" `is` (Accept $ MIME.Other "foo")
    describe "Accept Aac" do
      "unknown" `isnt` Accept Type.MIME.Aac
      "audio/aac" `is` Accept Type.MIME.Aac
    describe "Accept Abw" do
      "unknown" `isnt` Accept Type.MIME.Abw
      "application/x-abiword" `is` Accept Type.MIME.Abw
    describe "Accept Arc" do
      "unknown" `isnt` Accept Type.MIME.Arc
      "application/x-freearc" `is` Accept Type.MIME.Arc
    describe "Accept Avif" do
      "unknown" `isnt` Accept Type.MIME.Avif
      "image/avif" `is` Accept Type.MIME.Avif
    describe "Accept Avi" do
      "unknown" `isnt` Accept Type.MIME.Avi
      "video/x-msvideo" `is` Accept Type.MIME.Avi
    describe "Accept Azw" do
      "unknown" `isnt` Accept Type.MIME.Azw
      "application/vnd.amazon.ebook" `is` Accept Type.MIME.Azw
    describe "Accept Bin" do
      "unknown" `isnt` Accept Type.MIME.Bin
      "application/octet-stream" `is` Accept Type.MIME.Bin
    describe "Accept Bmp" do
      "unknown" `isnt` Accept Type.MIME.Bmp
      "image/bmp" `is` Accept Type.MIME.Bmp
    describe "Accept Bz" do
      "unknown" `isnt` Accept Type.MIME.Bz
      "application/x-bzip" `is` Accept Type.MIME.Bz
    describe "Accept Bz2" do
      "unknown" `isnt` Accept Type.MIME.Bz2
      "application/x-bzip2" `is` Accept Type.MIME.Bz2
    describe "Accept Cda" do
      "unknown" `isnt` Accept Type.MIME.Cda
      "application/x-cdf" `is` Accept Type.MIME.Cda
    describe "Accept Csh" do
      "unknown" `isnt` Accept Type.MIME.Csh
      "application/x-csh" `is` Accept Type.MIME.Csh
    describe "Accept Css" do
      "unknown" `isnt` Accept Type.MIME.Css
      "text/css" `is` Accept Type.MIME.Css
    describe "Accept Csv" do
      "unknown" `isnt` Accept Type.MIME.Csv
      "text/csv" `is` Accept Type.MIME.Csv
    describe "Accept Doc" do
      "unknown" `isnt` Accept Type.MIME.Doc
      "application/msword" `is` Accept Type.MIME.Doc
    describe "Accept Docx" do
      "unknown" `isnt` Accept Type.MIME.Docx
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" `is` Accept Type.MIME.Docx
    describe "Accept Eot" do
      "unknown" `isnt` Accept Type.MIME.Eot
      "application/vnd.ms-fontobject" `is` Accept Type.MIME.Eot
    describe "Accept Epub" do
      "unknown" `isnt` Accept Type.MIME.Epub
      "application/epub+zip" `is` Accept Type.MIME.Epub
    describe "Accept Gz" do
      "unknown" `isnt` Accept Type.MIME.Gz
      "application/gzip" `is` Accept Type.MIME.Gz
    describe "Accept Gif" do
      "unknown" `isnt` Accept Type.MIME.Gif
      "image/gif" `is` Accept Type.MIME.Gif
    describe "Accept Html" do
      "unknown" `isnt` Accept Type.MIME.Html
      "text/html" `is` Accept Type.MIME.Html
    describe "Accept Ico" do
      "unknown" `isnt` Accept Type.MIME.Ico
      "image/vnd.microsoft.icon" `is` Accept Type.MIME.Ico
    describe "Accept Ics" do
      "unknown" `isnt` Accept Type.MIME.Ics
      "text/calendar" `is` Accept Type.MIME.Ics
    describe "Accept Jar" do
      "unknown" `isnt` Accept Type.MIME.Jar
      "application/java-archive" `is` Accept Type.MIME.Jar
    describe "Accept Jpeg" do
      "unknown" `isnt` Accept Type.MIME.Jpeg
      "image/jpeg" `is` Accept Type.MIME.Jpeg
    describe "Accept Js" do
      "unknown" `isnt` Accept Type.MIME.Js
      "text/javascript" `is` Accept Type.MIME.Js
    describe "Accept Json" do
      "unknown" `isnt` Accept Type.MIME.Json
      "application/json" `is` Accept Type.MIME.Json
    describe "Accept Jsonld" do
      "unknown" `isnt` Accept Type.MIME.Jsonld
      "application/ld+json" `is` Accept Type.MIME.Jsonld
    describe "Accept Midi" do
      "unknown" `isnt` Accept Type.MIME.Midi
      "audio/midi" `is` Accept Type.MIME.Midi
    describe "Accept Mp3" do
      "unknown" `isnt` Accept Type.MIME.Mp3
      "audio/mpeg" `is` Accept Type.MIME.Mp3
    describe "Accept Mp4" do
      "unknown" `isnt` Accept Type.MIME.Mp4
      "video/mp4" `is` Accept Type.MIME.Mp4
    describe "Accept Mpeg" do
      "unknown" `isnt` Accept Type.MIME.Mpeg
      "video/mpeg" `is` Accept Type.MIME.Mpeg
    describe "Accept Mpkg" do
      "unknown" `isnt` Accept Type.MIME.Mpkg
      "application/vnd.apple.installer+xml" `is` Accept Type.MIME.Mpkg
    describe "Accept Odp" do
      "unknown" `isnt` Accept Type.MIME.Odp
      "application/vnd.oasis.opendocument.presentation" `is` Accept Type.MIME.Odp
    describe "Accept Ods" do
      "unknown" `isnt` Accept Type.MIME.Ods
      "application/vnd.oasis.opendocument.spreadsheet" `is` Accept Type.MIME.Ods
    describe "Accept Odt" do
      "unknown" `isnt` Accept Type.MIME.Odt
      "application/vnd.oasis.opendocument.text" `is` Accept Type.MIME.Odt
    describe "Accept Oga" do
      "unknown" `isnt` Accept Type.MIME.Oga
      "audio/ogg" `is` Accept Type.MIME.Oga
    describe "Accept Ogv" do
      "unknown" `isnt` Accept Type.MIME.Ogv
      "video/ogg" `is` Accept Type.MIME.Ogv
    describe "Accept Ogx" do
      "unknown" `isnt` Accept Type.MIME.Ogx
      "application/ogg" `is` Accept Type.MIME.Ogx
    describe "Accept Opus" do
      "unknown" `isnt` Accept Type.MIME.Opus
      "audio/opus" `is` Accept Type.MIME.Opus
    describe "Accept Otf" do
      "unknown" `isnt` Accept Type.MIME.Otf
      "font/otf" `is` Accept Type.MIME.Otf
    describe "Accept Png" do
      "unknown" `isnt` Accept Type.MIME.Png
      "image/png" `is` Accept Type.MIME.Png
    describe "Accept Pdf" do
      "unknown" `isnt` Accept Type.MIME.Pdf
      "application/pdf" `is` Accept Type.MIME.Pdf
    describe "Accept Php" do
      "unknown" `isnt` Accept Type.MIME.Php
      "application/x-httpd-php" `is` Accept Type.MIME.Php
    describe "Accept Ppt" do
      "unknown" `isnt` Accept Type.MIME.Ppt
      "application/vnd.ms-powerpoint" `is` Accept Type.MIME.Ppt
    describe "Accept Pptx" do
      "unknown" `isnt` Accept Type.MIME.Pptx
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" `is` Accept Type.MIME.Pptx
    describe "Accept Rar" do
      "unknown" `isnt` Accept Type.MIME.Rar
      "application/vnd.rar" `is` Accept Type.MIME.Rar
    describe "Accept Rtf" do
      "unknown" `isnt` Accept Type.MIME.Rtf
      "application/rtf" `is` Accept Type.MIME.Rtf
    describe "Accept Sh" do
      "unknown" `isnt` Accept Type.MIME.Sh
      "application/x-sh" `is` Accept Type.MIME.Sh
    describe "Accept Svg" do
      "unknown" `isnt` Accept Type.MIME.Svg
      "image/svg+xml" `is` Accept Type.MIME.Svg
    describe "Accept Tar" do
      "unknown" `isnt` Accept Type.MIME.Tar
      "application/x-tar" `is` Accept Type.MIME.Tar
    describe "Accept Tif" do
      "unknown" `isnt` Accept Type.MIME.Tif
      "image/tiff" `is` Accept Type.MIME.Tif
    describe "Accept Ts" do
      "unknown" `isnt` Accept Type.MIME.Ts
      "video/mp2t" `is` Accept Type.MIME.Ts
    describe "Accept Ttf" do
      "unknown" `isnt` Accept Type.MIME.Ttf
      "font/ttf" `is` Accept Type.MIME.Ttf
    describe "Accept Txt" do
      "unknown" `isnt` Accept Type.MIME.Txt
      "text/plain" `is` Accept Type.MIME.Txt
    describe "Accept Vsd" do
      "unknown" `isnt` Accept Type.MIME.Vsd
      "application/vnd.visio" `is` Accept Type.MIME.Vsd
    describe "Accept Wav" do
      "unknown" `isnt` Accept Type.MIME.Wav
      "audio/wav" `is` Accept Type.MIME.Wav
    describe "Accept Weba" do
      "unknown" `isnt` Accept Type.MIME.Weba
      "audio/webm" `is` Accept Type.MIME.Weba
    describe "Accept Webm" do
      "unknown" `isnt` Accept Type.MIME.Webm
      "video/webm" `is` Accept Type.MIME.Webm
    describe "Accept Webp" do
      "unknown" `isnt` Accept Type.MIME.Webp
      "image/webp" `is` Accept Type.MIME.Webp
    describe "Accept Woff" do
      "unknown" `isnt` Accept Type.MIME.Woff
      "font/woff" `is` Accept Type.MIME.Woff
    describe "Accept Woff2" do
      "unknown" `isnt` Accept Type.MIME.Woff2
      "font/woff2" `is` Accept Type.MIME.Woff2
    describe "Accept Xhtml" do
      "unknown" `isnt` Accept Type.MIME.Xhtml
      "application/xhtml+xml" `is` Accept Type.MIME.Xhtml
    describe "Accept Xls" do
      "unknown" `isnt` Accept Type.MIME.Xls
      "application/vnd.ms-excel" `is` Accept Type.MIME.Xls
    describe "Accept Xlsx" do
      "unknown" `isnt` Accept Type.MIME.Xlsx
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" `is` Accept Type.MIME.Xlsx
    describe "Accept Xml" do
      "unknown" `isnt` Accept Type.MIME.Xml
      "application/xml" `is` Accept Type.MIME.Xml
    describe "Accept Xul" do
      "unknown" `isnt` Accept Type.MIME.Xul
      "application/vnd.mozilla.xul+xml" `is` Accept Type.MIME.Xul
    describe "Accept Zip" do
      "unknown" `isnt` Accept Type.MIME.Zip
      "application/zip" `is` Accept Type.MIME.Zip
    describe "Accept Video3gp" do
      "unknown" `isnt` Accept Type.MIME.Video3gp
      "video/3gpp" `is` Accept Type.MIME.Video3gp
    describe "Accept Video3g2" do
      "unknown" `isnt` Accept Type.MIME.Video3g2
      "video/3gpp2" `is` Accept Type.MIME.Video3g2
    describe "Accept Archive7z" do
      "unknown" `isnt` Accept Type.MIME.Archive7z
      "application/x-7z-compressed" `is` Accept Type.MIME.Archive7z
    describe "ContentType String" do
      "foo" `is` (ContentType "foo")
      "" `is` (ContentType "")
    describe "ContentType MIME.MIME" do
      "application/json" `is` (ContentType MIME.Json)
      "text/plain" `is` (ContentType MIME.Txt)
      "text/plain;charset=utf8" `is` (ContentType $ MIME.Other "text/plain;charset=utf8")
      "foo" `is` (ContentType $ MIME.Other "foo")
    describe "ContentType Aac" do
      "unknown" `isnt` ContentType Type.MIME.Aac
      "audio/aac" `is` ContentType Type.MIME.Aac
    describe "ContentType Abw" do
      "unknown" `isnt` ContentType Type.MIME.Abw
      "application/x-abiword" `is` ContentType Type.MIME.Abw
    describe "ContentType Arc" do
      "unknown" `isnt` ContentType Type.MIME.Arc
      "application/x-freearc" `is` ContentType Type.MIME.Arc
    describe "ContentType Avif" do
      "unknown" `isnt` ContentType Type.MIME.Avif
      "image/avif" `is` ContentType Type.MIME.Avif
    describe "ContentType Avi" do
      "unknown" `isnt` ContentType Type.MIME.Avi
      "video/x-msvideo" `is` ContentType Type.MIME.Avi
    describe "ContentType Azw" do
      "unknown" `isnt` ContentType Type.MIME.Azw
      "application/vnd.amazon.ebook" `is` ContentType Type.MIME.Azw
    describe "ContentType Bin" do
      "unknown" `isnt` ContentType Type.MIME.Bin
      "application/octet-stream" `is` ContentType Type.MIME.Bin
    describe "ContentType Bmp" do
      "unknown" `isnt` ContentType Type.MIME.Bmp
      "image/bmp" `is` ContentType Type.MIME.Bmp
    describe "ContentType Bz" do
      "unknown" `isnt` ContentType Type.MIME.Bz
      "application/x-bzip" `is` ContentType Type.MIME.Bz
    describe "ContentType Bz2" do
      "unknown" `isnt` ContentType Type.MIME.Bz2
      "application/x-bzip2" `is` ContentType Type.MIME.Bz2
    describe "ContentType Cda" do
      "unknown" `isnt` ContentType Type.MIME.Cda
      "application/x-cdf" `is` ContentType Type.MIME.Cda
    describe "ContentType Csh" do
      "unknown" `isnt` ContentType Type.MIME.Csh
      "application/x-csh" `is` ContentType Type.MIME.Csh
    describe "ContentType Css" do
      "unknown" `isnt` ContentType Type.MIME.Css
      "text/css" `is` ContentType Type.MIME.Css
    describe "ContentType Csv" do
      "unknown" `isnt` ContentType Type.MIME.Csv
      "text/csv" `is` ContentType Type.MIME.Csv
    describe "ContentType Doc" do
      "unknown" `isnt` ContentType Type.MIME.Doc
      "application/msword" `is` ContentType Type.MIME.Doc
    describe "ContentType Docx" do
      "unknown" `isnt` ContentType Type.MIME.Docx
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" `is` ContentType Type.MIME.Docx
    describe "ContentType Eot" do
      "unknown" `isnt` ContentType Type.MIME.Eot
      "application/vnd.ms-fontobject" `is` ContentType Type.MIME.Eot
    describe "ContentType Epub" do
      "unknown" `isnt` ContentType Type.MIME.Epub
      "application/epub+zip" `is` ContentType Type.MIME.Epub
    describe "ContentType Gz" do
      "unknown" `isnt` ContentType Type.MIME.Gz
      "application/gzip" `is` ContentType Type.MIME.Gz
    describe "ContentType Gif" do
      "unknown" `isnt` ContentType Type.MIME.Gif
      "image/gif" `is` ContentType Type.MIME.Gif
    describe "ContentType Html" do
      "unknown" `isnt` ContentType Type.MIME.Html
      "text/html" `is` ContentType Type.MIME.Html
    describe "ContentType Ico" do
      "unknown" `isnt` ContentType Type.MIME.Ico
      "image/vnd.microsoft.icon" `is` ContentType Type.MIME.Ico
    describe "ContentType Ics" do
      "unknown" `isnt` ContentType Type.MIME.Ics
      "text/calendar" `is` ContentType Type.MIME.Ics
    describe "ContentType Jar" do
      "unknown" `isnt` ContentType Type.MIME.Jar
      "application/java-archive" `is` ContentType Type.MIME.Jar
    describe "ContentType Jpeg" do
      "unknown" `isnt` ContentType Type.MIME.Jpeg
      "image/jpeg" `is` ContentType Type.MIME.Jpeg
    describe "ContentType Js" do
      "unknown" `isnt` ContentType Type.MIME.Js
      "text/javascript" `is` ContentType Type.MIME.Js
    describe "ContentType Json" do
      "unknown" `isnt` ContentType Type.MIME.Json
      "application/json" `is` ContentType Type.MIME.Json
    describe "ContentType Jsonld" do
      "unknown" `isnt` ContentType Type.MIME.Jsonld
      "application/ld+json" `is` ContentType Type.MIME.Jsonld
    describe "ContentType Midi" do
      "unknown" `isnt` ContentType Type.MIME.Midi
      "audio/midi" `is` ContentType Type.MIME.Midi
    describe "ContentType Mp3" do
      "unknown" `isnt` ContentType Type.MIME.Mp3
      "audio/mpeg" `is` ContentType Type.MIME.Mp3
    describe "ContentType Mp4" do
      "unknown" `isnt` ContentType Type.MIME.Mp4
      "video/mp4" `is` ContentType Type.MIME.Mp4
    describe "ContentType Mpeg" do
      "unknown" `isnt` ContentType Type.MIME.Mpeg
      "video/mpeg" `is` ContentType Type.MIME.Mpeg
    describe "ContentType Mpkg" do
      "unknown" `isnt` ContentType Type.MIME.Mpkg
      "application/vnd.apple.installer+xml" `is` ContentType Type.MIME.Mpkg
    describe "ContentType Odp" do
      "unknown" `isnt` ContentType Type.MIME.Odp
      "application/vnd.oasis.opendocument.presentation" `is` ContentType Type.MIME.Odp
    describe "ContentType Ods" do
      "unknown" `isnt` ContentType Type.MIME.Ods
      "application/vnd.oasis.opendocument.spreadsheet" `is` ContentType Type.MIME.Ods
    describe "ContentType Odt" do
      "unknown" `isnt` ContentType Type.MIME.Odt
      "application/vnd.oasis.opendocument.text" `is` ContentType Type.MIME.Odt
    describe "ContentType Oga" do
      "unknown" `isnt` ContentType Type.MIME.Oga
      "audio/ogg" `is` ContentType Type.MIME.Oga
    describe "ContentType Ogv" do
      "unknown" `isnt` ContentType Type.MIME.Ogv
      "video/ogg" `is` ContentType Type.MIME.Ogv
    describe "ContentType Ogx" do
      "unknown" `isnt` ContentType Type.MIME.Ogx
      "application/ogg" `is` ContentType Type.MIME.Ogx
    describe "ContentType Opus" do
      "unknown" `isnt` ContentType Type.MIME.Opus
      "audio/opus" `is` ContentType Type.MIME.Opus
    describe "ContentType Otf" do
      "unknown" `isnt` ContentType Type.MIME.Otf
      "font/otf" `is` ContentType Type.MIME.Otf
    describe "ContentType Png" do
      "unknown" `isnt` ContentType Type.MIME.Png
      "image/png" `is` ContentType Type.MIME.Png
    describe "ContentType Pdf" do
      "unknown" `isnt` ContentType Type.MIME.Pdf
      "application/pdf" `is` ContentType Type.MIME.Pdf
    describe "ContentType Php" do
      "unknown" `isnt` ContentType Type.MIME.Php
      "application/x-httpd-php" `is` ContentType Type.MIME.Php
    describe "ContentType Ppt" do
      "unknown" `isnt` ContentType Type.MIME.Ppt
      "application/vnd.ms-powerpoint" `is` ContentType Type.MIME.Ppt
    describe "ContentType Pptx" do
      "unknown" `isnt` ContentType Type.MIME.Pptx
      "application/vnd.openxmlformats-officedocument.presentationml.presentation" `is` ContentType Type.MIME.Pptx
    describe "ContentType Rar" do
      "unknown" `isnt` ContentType Type.MIME.Rar
      "application/vnd.rar" `is` ContentType Type.MIME.Rar
    describe "ContentType Rtf" do
      "unknown" `isnt` ContentType Type.MIME.Rtf
      "application/rtf" `is` ContentType Type.MIME.Rtf
    describe "ContentType Sh" do
      "unknown" `isnt` ContentType Type.MIME.Sh
      "application/x-sh" `is` ContentType Type.MIME.Sh
    describe "ContentType Svg" do
      "unknown" `isnt` ContentType Type.MIME.Svg
      "image/svg+xml" `is` ContentType Type.MIME.Svg
    describe "ContentType Tar" do
      "unknown" `isnt` ContentType Type.MIME.Tar
      "application/x-tar" `is` ContentType Type.MIME.Tar
    describe "ContentType Tif" do
      "unknown" `isnt` ContentType Type.MIME.Tif
      "image/tiff" `is` ContentType Type.MIME.Tif
    describe "ContentType Ts" do
      "unknown" `isnt` ContentType Type.MIME.Ts
      "video/mp2t" `is` ContentType Type.MIME.Ts
    describe "ContentType Ttf" do
      "unknown" `isnt` ContentType Type.MIME.Ttf
      "font/ttf" `is` ContentType Type.MIME.Ttf
    describe "ContentType Txt" do
      "unknown" `isnt` ContentType Type.MIME.Txt
      "text/plain" `is` ContentType Type.MIME.Txt
    describe "ContentType Vsd" do
      "unknown" `isnt` ContentType Type.MIME.Vsd
      "application/vnd.visio" `is` ContentType Type.MIME.Vsd
    describe "ContentType Wav" do
      "unknown" `isnt` ContentType Type.MIME.Wav
      "audio/wav" `is` ContentType Type.MIME.Wav
    describe "ContentType Weba" do
      "unknown" `isnt` ContentType Type.MIME.Weba
      "audio/webm" `is` ContentType Type.MIME.Weba
    describe "ContentType Webm" do
      "unknown" `isnt` ContentType Type.MIME.Webm
      "video/webm" `is` ContentType Type.MIME.Webm
    describe "ContentType Webp" do
      "unknown" `isnt` ContentType Type.MIME.Webp
      "image/webp" `is` ContentType Type.MIME.Webp
    describe "ContentType Woff" do
      "unknown" `isnt` ContentType Type.MIME.Woff
      "font/woff" `is` ContentType Type.MIME.Woff
    describe "ContentType Woff2" do
      "unknown" `isnt` ContentType Type.MIME.Woff2
      "font/woff2" `is` ContentType Type.MIME.Woff2
    describe "ContentType Xhtml" do
      "unknown" `isnt` ContentType Type.MIME.Xhtml
      "application/xhtml+xml" `is` ContentType Type.MIME.Xhtml
    describe "ContentType Xls" do
      "unknown" `isnt` ContentType Type.MIME.Xls
      "application/vnd.ms-excel" `is` ContentType Type.MIME.Xls
    describe "ContentType Xlsx" do
      "unknown" `isnt` ContentType Type.MIME.Xlsx
      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" `is` ContentType Type.MIME.Xlsx
    describe "ContentType Xml" do
      "unknown" `isnt` ContentType Type.MIME.Xml
      "application/xml" `is` ContentType Type.MIME.Xml
    describe "ContentType Xul" do
      "unknown" `isnt` ContentType Type.MIME.Xul
      "application/vnd.mozilla.xul+xml" `is` ContentType Type.MIME.Xul
    describe "ContentType Zip" do
      "unknown" `isnt` ContentType Type.MIME.Zip
      "application/zip" `is` ContentType Type.MIME.Zip
    describe "ContentType Video3gp" do
      "unknown" `isnt` ContentType Type.MIME.Video3gp
      "video/3gpp" `is` ContentType Type.MIME.Video3gp
    describe "ContentType Video3g2" do
      "unknown" `isnt` ContentType Type.MIME.Video3g2
      "video/3gpp2" `is` ContentType Type.MIME.Video3g2
    describe "ContentType Archive7z" do
      "unknown" `isnt` ContentType Type.MIME.Archive7z
      "application/x-7z-compressed" `is` ContentType Type.MIME.Archive7z
    describe "AccessControlAllowCredentials" do
      "true" `is` AccessControlAllowCredentials
      "" `isnt` AccessControlAllowCredentials
      "false" `isnt` AccessControlAllowCredentials
    describe "AccessControlAllowHeaders" do
      "*" `is` AccessControlAllowHeaders (Left Wildcard)
      " * " `is` AccessControlAllowHeaders (Left Wildcard)
      "* " `is` AccessControlAllowHeaders (Left Wildcard)
      "Vary" `is` AccessControlAllowHeaders (Right $ pure $ String.Lower.fromString "Vary")
      " Vary" `isnt` AccessControlAllowHeaders (Right $ pure $ String.Lower.fromString "Vary")
      "Vary  " `isnt` AccessControlAllowHeaders (Right $ pure $ String.Lower.fromString "Vary")
      "Vary,   " `is` AccessControlAllowHeaders (Right $ pure $ String.Lower.fromString "Vary")
      "Accept, Vary, Content-Type" `is` AccessControlAllowHeaders (Right $ (pure "Accept" <> pure "Vary" <> pure "Content-Type") <#> String.Lower.fromString)
    describe "AccessControlAllowMethods" do
      "*" `is` AccessControlAllowMethods (Left Wildcard)
      " * " `is` AccessControlAllowMethods (Left Wildcard)
      "* " `is` AccessControlAllowMethods (Left Wildcard)
      "GET" `is` AccessControlAllowMethods (Right $ pure GET)
      "get" `isnt` AccessControlAllowMethods (Right $ pure GET)
      "GET,,,,,, PATCH" `is` AccessControlAllowMethods (Right $ pure GET <> pure PATCH)
      "   GET    ,     PATCH    " `isnt` AccessControlAllowMethods (Right $ pure GET <> pure PATCH)
    describe "AccessControlAllowOrigin" do
      "*" `is` AccessControlAllowOrigin (Left Wildcard)
      " * " `is` AccessControlAllowOrigin (Left Wildcard)
      "* " `is` AccessControlAllowOrigin (Left Wildcard)
      "foo" `is` AccessControlAllowOrigin (Right "foo")
      "  foo    " `is` AccessControlAllowOrigin (Right "foo")
      "https://example.com" `is` AccessControlAllowOrigin (Right "https://example.com")
    describe "AccessControlExposeHeaders" do
      "*" `is` AccessControlExposeHeaders (Left Wildcard)
      " * " `is` AccessControlExposeHeaders (Left Wildcard)
      "* " `is` AccessControlExposeHeaders (Left Wildcard)
      "Vary" `is` AccessControlExposeHeaders (Right $ pure $ String.Lower.fromString "Vary")
      "Vary" `is` AccessControlExposeHeaders (Right $ pure $ String.Lower.fromString "Vary")
      "Vary  " `is` AccessControlExposeHeaders (Right $ pure $ String.Lower.fromString "Vary")
      "Accept, Vary, Content-Type" `is` AccessControlExposeHeaders (Right $ (pure "Accept" <> pure "Vary" <> pure "Content-Type") <#> String.Lower.fromString)
    describe "AccessControlMaxAge" do
      "  123 " `is` AccessControlMaxAge 123
      "  0" `is` AccessControlMaxAge 0
      "23190" `is` AccessControlMaxAge 23190
    describe "AccessControlRequestHeaders" do
      "Vary" `is` AccessControlRequestHeaders (pure $ String.Lower.fromString "Vary")
      " Vary" `is` AccessControlRequestHeaders (pure $ String.Lower.fromString "Vary")
      " Vary  " `is` AccessControlRequestHeaders (pure $ String.Lower.fromString "Vary")
      "Accept, Vary, Content-Type" `is` AccessControlRequestHeaders ((pure "Accept" <> pure "Vary" <> pure "Content-Type") <#> String.Lower.fromString)
    describe "AccessControlRequestMethod" do
      "GET" `is` AccessControlRequestMethod GET
      "   PATCH " `isnt` AccessControlRequestMethod PATCH
      "get" `isnt` AccessControlRequestMethod GET
      "PATCh" `isnt` AccessControlRequestMethod PATCH
    describe "Age" do
      "  123 " `is` Age 123
      "  0" `is` Age 0
      "23190" `is` Age 23190
    describe "Allow" do
      "GET" `is` Allow (pure GET)
      "get" `isnt` Allow (pure GET)
      "GET, PATCH" `is` Allow (pure GET <> pure PATCH)
      "GET,,,,,,  , ,   ,PATCH" `is` Allow (pure GET <> pure PATCH)
      "   GET    ,     PATCH    " `isnt` Allow (pure GET <> pure PATCH)
    describe "Authorization" do
      "Foo bar" `is` Authorization (AuthScheme "Foo") "bar"
      "Bing bar" `is` Authorization (AuthScheme "Bing") "bar"
      " Bing bar" `isnt` Authorization (AuthScheme "Bing") "bar"
      "Bing bar " `is` Authorization (AuthScheme "Bing") "bar"
      "  Bing bar    " `isnt` Authorization (AuthScheme "Bing") "bar"
      "Bar" `is` Authorization (AuthScheme "Bar") ""
    describe "BasicAuth" do
      "Basic ZGVtbzpwQDU1dzByZA==" `is` BasicAuth {username: "demo", password: "p@55w0rd"}
      "Bearer ZGVtbzpwQDU1dzByZA==" `isnt` BasicAuth {username: "demo", password: "p@55w0rd"}
      "Basic foo" `isnt` BasicAuth {username: "demo", password: "p@55w0rd"}
    describe "BearerAuth" do
      "Bearer foo" `is` BearerAuth "foo"
      "Basic foo" `isnt` BearerAuth "foo"
      "Bearer foo    " `is` BearerAuth "foo"
    describe "CacheControl" do
      "max-age=604800" `is` CacheControl (cacheControlDefaults {maxAge = Just 604800})
      "   max-age=604800" `isnt` CacheControl (cacheControlDefaults {maxAge = Just 604800})
      "max-age=604800 " `isnt` CacheControl (cacheControlDefaults {maxAge = Just 604800})
      "max-age=604800, must-revalidate" `is` CacheControl (cacheControlDefaults {maxAge = Just 604800, mustRevalidate = true})
      "max-age=20, s-maxage=10, no-cache, must-revalidate, proxy-revalidate, no-store, private, public, must-understand, no-transform, immutable, stale-while-revalidate, stale-if-error"
        `is`
          CacheControl (cacheControlDefaults {maxAge = Just 20, sMaxAge = Just 10, noCache = true, mustRevalidate = true, proxyRevalidate = true, noStore = true, private = true, public = true, mustUnderstand = true, noTransform = true, immutable = true, staleWhileRevalidate = true, staleIfError = true})
      "" `is` CacheControl cacheControlDefaults
      "    " `isnt` CacheControl cacheControlDefaults
      "foo=bar" `is` CacheControl cacheControlDefaults
      "foo" `is` CacheControl cacheControlDefaults
      "foo=bar " `isnt` CacheControl cacheControlDefaults
      "   foo " `isnt` CacheControl cacheControlDefaults
    describe "Connection" do
      "" `isnt` Connection (Left ConnectionClose)
      "   " `isnt` Connection (Left ConnectionClose)
      "close" `is` Connection (Left ConnectionClose)
      "  close  " `is` Connection (Left ConnectionClose)
      "  cLoSe  " `is` Connection (Left ConnectionClose)
      "fuaiowf" `is` Connection (Right $ pure $ String.Lower.fromString "fuaiowf")
      "  a   , b ,   c,d" `is` Connection (Right $ String.Lower.fromString <$> (pure "a" <> pure "b" <> pure "c" <> pure "d"))
    describe "ContentDisposition" do
      "form-data" `is` ContentDisposition (Either.Nested.in3 $ ContentDispositionFormData {filename: Nothing, name: Nothing})
      "form-data; name=\"foo\"" `is` ContentDisposition (Either.Nested.in3 $ ContentDispositionFormData {filename: Nothing, name: Just "foo"})
      "form-data; filename=\"foo.txt\"" `is` ContentDisposition (Either.Nested.in3 $ ContentDispositionFormData {filename: Just "foo.txt", name: Nothing})
      "   form-data; filename=\"foo.txt\"   ;    name=\"foo\"   " `isnt` ContentDisposition (Either.Nested.in3 $ ContentDispositionFormData {filename: Just "foo.txt", name: Just "foo"})

      "attachment" `is` ContentDisposition (Either.Nested.in2 $ ContentDispositionAttachment {filename: Nothing})
      "attachment; filename=\"foo.txt\"" `is` ContentDisposition (Either.Nested.in2 $ ContentDispositionAttachment {filename: Just "foo.txt"})
      "   attachment; filename=\"foo.txt\"     " `isnt` ContentDisposition (Either.Nested.in2 $ ContentDispositionAttachment {filename: Just "foo.txt"})

      "inline" `is` ContentDisposition (Either.Nested.in1 $ ContentDispositionInline)
      "inline " `is` ContentDisposition (Either.Nested.in1 $ ContentDispositionInline)
      "   inline    " `is` ContentDisposition (Either.Nested.in1 $ ContentDispositionInline)
    describe "ContentEncoding" do
      "gzip" `is` ContentEncoding (pure "gzip")
      "  gzip  " `is` ContentEncoding (pure "gzip")
      "  gzip  , deflate   " `is` ContentEncoding (pure "gzip" <> pure "deflate")
    describe "ContentLength" do
      "  0    " `is` ContentLength 0
      "  1    " `is` ContentLength 1
      "  1212943817    " `is` ContentLength 1212943817
    describe "ContentLocation" do
      "" `is` ContentLocation ""
      "a" `is` ContentLocation "a"
      "  a " `is` ContentLocation "a"
      "abc" `is` ContentLocation "abc"
    describe "ContentRange" do
      "bytes 0-10/10" `is` (ContentRange $ Either.Nested.in1 $ ByteRangeStart 0 /\ ByteRangeEnd 10 /\ ByteRangeLength 10)
      "  bytes   0-10/10  " `is` (ContentRange $ Either.Nested.in1 $ ByteRangeStart 0 /\ ByteRangeEnd 10 /\ ByteRangeLength 10)
      "  bytes   0-0/0  " `is` (ContentRange $ Either.Nested.in1 $ ByteRangeStart 0 /\ ByteRangeEnd 0 /\ ByteRangeLength 0)
      "bytes 0-10/*" `is` (ContentRange $ Either.Nested.in2 $ ByteRangeStart 0 /\ ByteRangeEnd 10)
      "  bytes    0-10/*  " `is` (ContentRange $ Either.Nested.in2 $ ByteRangeStart 0 /\ ByteRangeEnd 10)
      "bytes */10" `is` (ContentRange $ Either.Nested.in3 $ ByteRangeLength 10)
      "  bytes   */10   " `is` (ContentRange $ Either.Nested.in3 $ ByteRangeLength 10)
    describe "Cookie" do
      "foo=" `is` Cookie (pure ("foo" /\ ""))
      "foo=bar" `is` Cookie (pure ("foo" /\ "bar"))
      "foo=bar; baz=" `is` Cookie (pure ("foo" /\ "bar") <> pure ("baz" /\ ""))
      "foo=bar; baz=quux" `is` Cookie (pure ("foo" /\ "bar") <> pure ("baz" /\ "quux"))
    describe "Date" $ pure unit
    describe "ETag" $ pure unit
    describe "ExpectContinue" $ pure unit
    describe "Expires" $ pure unit
    describe "Host" $ pure unit
    describe "IfMatch" $ pure unit
    describe "IfNoneMatch" $ pure unit
    describe "IfModifiedSince" $ pure unit
    describe "IfRange" $ pure unit
    describe "IfUnmodifiedSince" $ pure unit
    describe "LastModified" $ pure unit
    describe "Origin" $ pure unit
    describe "ProxyAuthorization" $ pure unit
    describe "Range" $ pure unit
    describe "Referer" $ pure unit
    describe "ReferrerPolicy" $ pure unit
    describe "RetryAfter" $ pure unit
    describe "SecWebsocketKey" $ pure unit
    describe "SecWebsocketAccept" $ pure unit
    describe "SecWebsocketVersion" $ pure unit
    describe "Server" $ pure unit
    describe "SetCookie" $ pure unit
    describe "StrictTransportSecurity" $ pure unit
    describe "TE" $ pure unit
    describe "TransferEncoding" $ pure unit
    describe "Upgrade" $ pure unit
    describe "UserAgent" $ pure unit
    describe "Vary"$ pure unit
