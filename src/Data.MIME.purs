module Data.MIME where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data MIME = Other String | Aac | Abw | Arc | Avif | Avi | Azw | Bin | Bmp | Bz | Bz2 | Cda | Csh | Css | Csv | Doc | Docx | Eot | Epub | Gz | Gif | Html | Ico | Ics | Jar | Jpeg | Js | Json | Jsonld | Midi | Mp3 | Mp4 | Mpeg | Mpkg | Odp | Ods | Odt | Oga | Ogv | Ogx | Opus | Otf | Png | Pdf | Php | Ppt | Pptx | Rar | Rtf | Sh | Svg | Tar | Tif | Ts | Ttf | Txt | Vsd | Wav | Weba | Webm | Webp | Woff | Woff2 | Xhtml | Xls | Xlsx | Xml | Xul | Zip | Video3gp | Video3g2 | Archive7z

derive instance Generic MIME _
instance Show MIME where
  show = genericShow

instance Eq MIME where
  eq = genericEq

toString :: MIME -> String
toString (Other s) = s
toString Aac = "audio/aac"
toString Abw = "application/x-abiword"
toString Arc = "application/x-freearc"
toString Avif = "image/avif"
toString Avi = "video/x-msvideo"
toString Azw = "application/vnd.amazon.ebook"
toString Bin = "application/octet-stream"
toString Bmp = "image/bmp"
toString Bz = "application/x-bzip"
toString Bz2 = "application/x-bzip2"
toString Cda = "application/x-cdf"
toString Csh = "application/x-csh"
toString Css = "text/css"
toString Csv = "text/csv"
toString Doc = "application/msword"
toString Docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
toString Eot = "application/vnd.ms-fontobject"
toString Epub = "application/epub+zip"
toString Gz = "application/gzip"
toString Gif = "image/gif"
toString Html = "text/html"
toString Ico = "image/vnd.microsoft.icon"
toString Ics = "text/calendar"
toString Jar = "application/java-archive"
toString Jpeg = "image/jpeg"
toString Js = "text/javascript"
toString Json = "application/json"
toString Jsonld = "application/ld+json"
toString Midi = "audio/midi"
toString Mp3 = "audio/mpeg"
toString Mp4 = "video/mp4"
toString Mpeg = "video/mpeg"
toString Mpkg = "application/vnd.apple.installer+xml"
toString Odp = "application/vnd.oasis.opendocument.presentation"
toString Ods = "application/vnd.oasis.opendocument.spreadsheet"
toString Odt = "application/vnd.oasis.opendocument.text"
toString Oga = "audio/ogg"
toString Ogv = "video/ogg"
toString Ogx = "application/ogg"
toString Opus = "audio/opus"
toString Otf = "font/otf"
toString Png = "image/png"
toString Pdf = "application/pdf"
toString Php = "application/x-httpd-php"
toString Ppt = "application/vnd.ms-powerpoint"
toString Pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
toString Rar = "application/vnd.rar"
toString Rtf = "application/rtf"
toString Sh = "application/x-sh"
toString Svg = "image/svg+xml"
toString Tar = "application/x-tar"
toString Tif = "image/tiff"
toString Ts = "video/mp2t"
toString Ttf = "font/ttf"
toString Txt = "text/plain"
toString Vsd = "application/vnd.visio"
toString Wav = "audio/wav"
toString Weba = "audio/webm"
toString Webm = "video/webm"
toString Webp = "image/webp"
toString Woff = "font/woff"
toString Woff2 = "font/woff2"
toString Xhtml = "application/xhtml+xml"
toString Xls = "application/vnd.ms-excel"
toString Xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
toString Xml = "application/xml"
toString Xul = "application/vnd.mozilla.xul+xml"
toString Zip = "application/zip"
toString Video3gp = "video/3gpp"
toString Video3g2 = "video/3gpp2"
toString Archive7z = "application/x-7z-compressed"

fromString :: String -> MIME
fromString "audio/aac" = Aac
fromString "application/x-abiword" = Abw
fromString "application/x-freearc" = Arc
fromString "image/avif" = Avif
fromString "video/x-msvideo" = Avi
fromString "application/vnd.amazon.ebook" = Azw
fromString "application/octet-stream" = Bin
fromString "image/bmp" = Bmp
fromString "application/x-bzip" = Bz
fromString "application/x-bzip2" = Bz2
fromString "application/x-cdf" = Cda
fromString "application/x-csh" = Csh
fromString "text/css" = Css
fromString "text/csv" = Csv
fromString "application/msword" = Doc
fromString "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = Docx
fromString "application/vnd.ms-fontobject" = Eot
fromString "application/epub+zip" = Epub
fromString "application/gzip" = Gz
fromString "image/gif" = Gif
fromString "text/html" = Html
fromString "image/vnd.microsoft.icon" = Ico
fromString "text/calendar" = Ics
fromString "application/java-archive" = Jar
fromString "image/jpeg" = Jpeg
fromString "text/javascript" = Js
fromString "application/json" = Json
fromString "application/ld+json" = Jsonld
fromString "audio/midi" = Midi
fromString "audio/mpeg" = Mp3
fromString "video/mp4" = Mp4
fromString "video/mpeg" = Mpeg
fromString "application/vnd.apple.installer+xml" = Mpkg
fromString "application/vnd.oasis.opendocument.presentation" = Odp
fromString "application/vnd.oasis.opendocument.spreadsheet" = Ods
fromString "application/vnd.oasis.opendocument.text" = Odt
fromString "audio/ogg" = Oga
fromString "video/ogg" = Ogv
fromString "application/ogg" = Ogx
fromString "audio/opus" = Opus
fromString "font/otf" = Otf
fromString "image/png" = Png
fromString "application/pdf" = Pdf
fromString "application/x-httpd-php" = Php
fromString "application/vnd.ms-powerpoint" = Ppt
fromString "application/vnd.openxmlformats-officedocument.presentationml.presentation" = Pptx
fromString "application/vnd.rar" = Rar
fromString "application/rtf" = Rtf
fromString "application/x-sh" = Sh
fromString "image/svg+xml" = Svg
fromString "application/x-tar" = Tar
fromString "image/tiff" = Tif
fromString "video/mp2t" = Ts
fromString "font/ttf" = Ttf
fromString "text/plain" = Txt
fromString "application/vnd.visio" = Vsd
fromString "audio/wav" = Wav
fromString "audio/webm" = Weba
fromString "video/webm" = Webm
fromString "image/webp" = Webp
fromString "font/woff" = Woff
fromString "font/woff2" = Woff2
fromString "application/xhtml+xml" = Xhtml
fromString "application/vnd.ms-excel" = Xls
fromString "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = Xlsx
fromString "application/xml" = Xml
fromString "application/vnd.mozilla.xul+xml" = Xul
fromString "application/zip" = Zip
fromString "video/3gpp" = Video3gp
fromString "video/3gpp2" = Video3g2
fromString "application/x-7z-compressed" = Archive7z
fromString s = Other s

