module Type.MIME where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.MIME as MIME
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)

class TypelevelMIME a where
  fromValue :: MIME.MIME -> Maybe a
  value :: MIME.MIME

data Aac = Aac

derive instance Generic Aac _
derive instance Eq Aac
instance Show Aac where
  show = genericShow

instance TypelevelMIME Aac where
  fromValue MIME.Aac = Just Aac
  fromValue _ = Nothing
  value = MIME.Aac

data Abw = Abw

derive instance Generic Abw _
derive instance Eq Abw
instance Show Abw where
  show = genericShow

instance TypelevelMIME Abw where
  fromValue MIME.Abw = Just Abw
  fromValue _ = Nothing
  value = MIME.Abw

data Arc = Arc

derive instance Generic Arc _
derive instance Eq Arc
instance Show Arc where
  show = genericShow

instance TypelevelMIME Arc where
  fromValue MIME.Arc = Just Arc
  fromValue _ = Nothing
  value = MIME.Arc

data Avif = Avif

derive instance Generic Avif _
derive instance Eq Avif
instance Show Avif where
  show = genericShow

instance TypelevelMIME Avif where
  fromValue MIME.Avif = Just Avif
  fromValue _ = Nothing
  value = MIME.Avif

data Avi = Avi

derive instance Generic Avi _
derive instance Eq Avi
instance Show Avi where
  show = genericShow

instance TypelevelMIME Avi where
  fromValue MIME.Avi = Just Avi
  fromValue _ = Nothing
  value = MIME.Avi

data Azw = Azw

derive instance Generic Azw _
derive instance Eq Azw
instance Show Azw where
  show = genericShow

instance TypelevelMIME Azw where
  fromValue MIME.Azw = Just Azw
  fromValue _ = Nothing
  value = MIME.Azw

data Bin = Bin

derive instance Generic Bin _
derive instance Eq Bin
instance Show Bin where
  show = genericShow

instance TypelevelMIME Bin where
  fromValue MIME.Bin = Just Bin
  fromValue _ = Nothing
  value = MIME.Bin

data Bmp = Bmp

derive instance Generic Bmp _
derive instance Eq Bmp
instance Show Bmp where
  show = genericShow

instance TypelevelMIME Bmp where
  fromValue MIME.Bmp = Just Bmp
  fromValue _ = Nothing
  value = MIME.Bmp

data Bz = Bz

derive instance Generic Bz _
derive instance Eq Bz
instance Show Bz where
  show = genericShow

instance TypelevelMIME Bz where
  fromValue MIME.Bz = Just Bz
  fromValue _ = Nothing
  value = MIME.Bz

data Bz2 = Bz2

derive instance Generic Bz2 _
derive instance Eq Bz2
instance Show Bz2 where
  show = genericShow

instance TypelevelMIME Bz2 where
  fromValue MIME.Bz2 = Just Bz2
  fromValue _ = Nothing
  value = MIME.Bz2

data Cda = Cda

derive instance Generic Cda _
derive instance Eq Cda
instance Show Cda where
  show = genericShow

instance TypelevelMIME Cda where
  fromValue MIME.Cda = Just Cda
  fromValue _ = Nothing
  value = MIME.Cda

data Csh = Csh

derive instance Generic Csh _
derive instance Eq Csh
instance Show Csh where
  show = genericShow

instance TypelevelMIME Csh where
  fromValue MIME.Csh = Just Csh
  fromValue _ = Nothing
  value = MIME.Csh

data Css = Css

derive instance Generic Css _
derive instance Eq Css
instance Show Css where
  show = genericShow

instance TypelevelMIME Css where
  fromValue MIME.Css = Just Css
  fromValue _ = Nothing
  value = MIME.Css

data Csv = Csv

derive instance Generic Csv _
derive instance Eq Csv
instance Show Csv where
  show = genericShow

instance TypelevelMIME Csv where
  fromValue MIME.Csv = Just Csv
  fromValue _ = Nothing
  value = MIME.Csv

data Doc = Doc

derive instance Generic Doc _
derive instance Eq Doc
instance Show Doc where
  show = genericShow

instance TypelevelMIME Doc where
  fromValue MIME.Doc = Just Doc
  fromValue _ = Nothing
  value = MIME.Doc

data Docx = Docx

derive instance Generic Docx _
derive instance Eq Docx
instance Show Docx where
  show = genericShow

instance TypelevelMIME Docx where
  fromValue MIME.Docx = Just Docx
  fromValue _ = Nothing
  value = MIME.Docx

data Eot = Eot

derive instance Generic Eot _
derive instance Eq Eot
instance Show Eot where
  show = genericShow

instance TypelevelMIME Eot where
  fromValue MIME.Eot = Just Eot
  fromValue _ = Nothing
  value = MIME.Eot

data Epub = Epub

derive instance Generic Epub _
derive instance Eq Epub
instance Show Epub where
  show = genericShow

instance TypelevelMIME Epub where
  fromValue MIME.Epub = Just Epub
  fromValue _ = Nothing
  value = MIME.Epub

data Gz = Gz

derive instance Generic Gz _
derive instance Eq Gz
instance Show Gz where
  show = genericShow

instance TypelevelMIME Gz where
  fromValue MIME.Gz = Just Gz
  fromValue _ = Nothing
  value = MIME.Gz

data Gif = Gif

derive instance Generic Gif _
derive instance Eq Gif
instance Show Gif where
  show = genericShow

instance TypelevelMIME Gif where
  fromValue MIME.Gif = Just Gif
  fromValue _ = Nothing
  value = MIME.Gif

data Html = Html

derive instance Generic Html _
derive instance Eq Html
instance Show Html where
  show = genericShow

instance TypelevelMIME Html where
  fromValue MIME.Html = Just Html
  fromValue _ = Nothing
  value = MIME.Html

data Ico = Ico

derive instance Generic Ico _
derive instance Eq Ico
instance Show Ico where
  show = genericShow

instance TypelevelMIME Ico where
  fromValue MIME.Ico = Just Ico
  fromValue _ = Nothing
  value = MIME.Ico

data Ics = Ics

derive instance Generic Ics _
derive instance Eq Ics
instance Show Ics where
  show = genericShow

instance TypelevelMIME Ics where
  fromValue MIME.Ics = Just Ics
  fromValue _ = Nothing
  value = MIME.Ics

data Jar = Jar

derive instance Generic Jar _
derive instance Eq Jar
instance Show Jar where
  show = genericShow

instance TypelevelMIME Jar where
  fromValue MIME.Jar = Just Jar
  fromValue _ = Nothing
  value = MIME.Jar

data Jpeg = Jpeg

derive instance Generic Jpeg _
derive instance Eq Jpeg
instance Show Jpeg where
  show = genericShow

instance TypelevelMIME Jpeg where
  fromValue MIME.Jpeg = Just Jpeg
  fromValue _ = Nothing
  value = MIME.Jpeg

data Js = Js

derive instance Generic Js _
derive instance Eq Js
instance Show Js where
  show = genericShow

instance TypelevelMIME Js where
  fromValue MIME.Js = Just Js
  fromValue _ = Nothing
  value = MIME.Js

data Json = Json

derive instance Generic Json _
derive instance Eq Json
instance Show Json where
  show = genericShow

instance TypelevelMIME Json where
  fromValue MIME.Json = Just Json
  fromValue _ = Nothing
  value = MIME.Json

data Jsonld = Jsonld

derive instance Generic Jsonld _
derive instance Eq Jsonld
instance Show Jsonld where
  show = genericShow

instance TypelevelMIME Jsonld where
  fromValue MIME.Jsonld = Just Jsonld
  fromValue _ = Nothing
  value = MIME.Jsonld

data Midi = Midi

derive instance Generic Midi _
derive instance Eq Midi
instance Show Midi where
  show = genericShow

instance TypelevelMIME Midi where
  fromValue MIME.Midi = Just Midi
  fromValue _ = Nothing
  value = MIME.Midi

data Mp3 = Mp3

derive instance Generic Mp3 _
derive instance Eq Mp3
instance Show Mp3 where
  show = genericShow

instance TypelevelMIME Mp3 where
  fromValue MIME.Mp3 = Just Mp3
  fromValue _ = Nothing
  value = MIME.Mp3

data Mp4 = Mp4

derive instance Generic Mp4 _
derive instance Eq Mp4
instance Show Mp4 where
  show = genericShow

instance TypelevelMIME Mp4 where
  fromValue MIME.Mp4 = Just Mp4
  fromValue _ = Nothing
  value = MIME.Mp4

data Mpeg = Mpeg

derive instance Generic Mpeg _
derive instance Eq Mpeg
instance Show Mpeg where
  show = genericShow

instance TypelevelMIME Mpeg where
  fromValue MIME.Mpeg = Just Mpeg
  fromValue _ = Nothing
  value = MIME.Mpeg

data Mpkg = Mpkg

derive instance Generic Mpkg _
derive instance Eq Mpkg
instance Show Mpkg where
  show = genericShow

instance TypelevelMIME Mpkg where
  fromValue MIME.Mpkg = Just Mpkg
  fromValue _ = Nothing
  value = MIME.Mpkg

data Odp = Odp

derive instance Generic Odp _
derive instance Eq Odp
instance Show Odp where
  show = genericShow

instance TypelevelMIME Odp where
  fromValue MIME.Odp = Just Odp
  fromValue _ = Nothing
  value = MIME.Odp

data Ods = Ods

derive instance Generic Ods _
derive instance Eq Ods
instance Show Ods where
  show = genericShow

instance TypelevelMIME Ods where
  fromValue MIME.Ods = Just Ods
  fromValue _ = Nothing
  value = MIME.Ods

data Odt = Odt

derive instance Generic Odt _
derive instance Eq Odt
instance Show Odt where
  show = genericShow

instance TypelevelMIME Odt where
  fromValue MIME.Odt = Just Odt
  fromValue _ = Nothing
  value = MIME.Odt

data Oga = Oga

derive instance Generic Oga _
derive instance Eq Oga
instance Show Oga where
  show = genericShow

instance TypelevelMIME Oga where
  fromValue MIME.Oga = Just Oga
  fromValue _ = Nothing
  value = MIME.Oga

data Ogv = Ogv

derive instance Generic Ogv _
derive instance Eq Ogv
instance Show Ogv where
  show = genericShow

instance TypelevelMIME Ogv where
  fromValue MIME.Ogv = Just Ogv
  fromValue _ = Nothing
  value = MIME.Ogv

data Ogx = Ogx

derive instance Generic Ogx _
derive instance Eq Ogx
instance Show Ogx where
  show = genericShow

instance TypelevelMIME Ogx where
  fromValue MIME.Ogx = Just Ogx
  fromValue _ = Nothing
  value = MIME.Ogx

data Opus = Opus

derive instance Generic Opus _
derive instance Eq Opus
instance Show Opus where
  show = genericShow

instance TypelevelMIME Opus where
  fromValue MIME.Opus = Just Opus
  fromValue _ = Nothing
  value = MIME.Opus

data Otf = Otf

derive instance Generic Otf _
derive instance Eq Otf
instance Show Otf where
  show = genericShow

instance TypelevelMIME Otf where
  fromValue MIME.Otf = Just Otf
  fromValue _ = Nothing
  value = MIME.Otf

data Png = Png

derive instance Generic Png _
derive instance Eq Png
instance Show Png where
  show = genericShow

instance TypelevelMIME Png where
  fromValue MIME.Png = Just Png
  fromValue _ = Nothing
  value = MIME.Png

data Pdf = Pdf

derive instance Generic Pdf _
derive instance Eq Pdf
instance Show Pdf where
  show = genericShow

instance TypelevelMIME Pdf where
  fromValue MIME.Pdf = Just Pdf
  fromValue _ = Nothing
  value = MIME.Pdf

data Php = Php

derive instance Generic Php _
derive instance Eq Php
instance Show Php where
  show = genericShow

instance TypelevelMIME Php where
  fromValue MIME.Php = Just Php
  fromValue _ = Nothing
  value = MIME.Php

data Ppt = Ppt

derive instance Generic Ppt _
derive instance Eq Ppt
instance Show Ppt where
  show = genericShow

instance TypelevelMIME Ppt where
  fromValue MIME.Ppt = Just Ppt
  fromValue _ = Nothing
  value = MIME.Ppt

data Pptx = Pptx

derive instance Generic Pptx _
derive instance Eq Pptx
instance Show Pptx where
  show = genericShow

instance TypelevelMIME Pptx where
  fromValue MIME.Pptx = Just Pptx
  fromValue _ = Nothing
  value = MIME.Pptx

data Rar = Rar

derive instance Generic Rar _
derive instance Eq Rar
instance Show Rar where
  show = genericShow

instance TypelevelMIME Rar where
  fromValue MIME.Rar = Just Rar
  fromValue _ = Nothing
  value = MIME.Rar

data Rtf = Rtf

derive instance Generic Rtf _
derive instance Eq Rtf
instance Show Rtf where
  show = genericShow

instance TypelevelMIME Rtf where
  fromValue MIME.Rtf = Just Rtf
  fromValue _ = Nothing
  value = MIME.Rtf

data Sh = Sh

derive instance Generic Sh _
derive instance Eq Sh
instance Show Sh where
  show = genericShow

instance TypelevelMIME Sh where
  fromValue MIME.Sh = Just Sh
  fromValue _ = Nothing
  value = MIME.Sh

data Svg = Svg

derive instance Generic Svg _
derive instance Eq Svg
instance Show Svg where
  show = genericShow

instance TypelevelMIME Svg where
  fromValue MIME.Svg = Just Svg
  fromValue _ = Nothing
  value = MIME.Svg

data Tar = Tar

derive instance Generic Tar _
derive instance Eq Tar
instance Show Tar where
  show = genericShow

instance TypelevelMIME Tar where
  fromValue MIME.Tar = Just Tar
  fromValue _ = Nothing
  value = MIME.Tar

data Tif = Tif

derive instance Generic Tif _
derive instance Eq Tif
instance Show Tif where
  show = genericShow

instance TypelevelMIME Tif where
  fromValue MIME.Tif = Just Tif
  fromValue _ = Nothing
  value = MIME.Tif

data Ts = Ts

derive instance Generic Ts _
derive instance Eq Ts
instance Show Ts where
  show = genericShow

instance TypelevelMIME Ts where
  fromValue MIME.Ts = Just Ts
  fromValue _ = Nothing
  value = MIME.Ts

data Ttf = Ttf

derive instance Generic Ttf _
derive instance Eq Ttf
instance Show Ttf where
  show = genericShow

instance TypelevelMIME Ttf where
  fromValue MIME.Ttf = Just Ttf
  fromValue _ = Nothing
  value = MIME.Ttf

data Txt = Txt

derive instance Generic Txt _
derive instance Eq Txt
instance Show Txt where
  show = genericShow

instance TypelevelMIME Txt where
  fromValue MIME.Txt = Just Txt
  fromValue _ = Nothing
  value = MIME.Txt

data Vsd = Vsd

derive instance Generic Vsd _
derive instance Eq Vsd
instance Show Vsd where
  show = genericShow

instance TypelevelMIME Vsd where
  fromValue MIME.Vsd = Just Vsd
  fromValue _ = Nothing
  value = MIME.Vsd

data Wav = Wav

derive instance Generic Wav _
derive instance Eq Wav
instance Show Wav where
  show = genericShow

instance TypelevelMIME Wav where
  fromValue MIME.Wav = Just Wav
  fromValue _ = Nothing
  value = MIME.Wav

data Weba = Weba

derive instance Generic Weba _
derive instance Eq Weba
instance Show Weba where
  show = genericShow

instance TypelevelMIME Weba where
  fromValue MIME.Weba = Just Weba
  fromValue _ = Nothing
  value = MIME.Weba

data Webm = Webm

derive instance Generic Webm _
derive instance Eq Webm
instance Show Webm where
  show = genericShow

instance TypelevelMIME Webm where
  fromValue MIME.Webm = Just Webm
  fromValue _ = Nothing
  value = MIME.Webm

data Webp = Webp

derive instance Generic Webp _
derive instance Eq Webp
instance Show Webp where
  show = genericShow

instance TypelevelMIME Webp where
  fromValue MIME.Webp = Just Webp
  fromValue _ = Nothing
  value = MIME.Webp

data Woff = Woff

derive instance Generic Woff _
derive instance Eq Woff
instance Show Woff where
  show = genericShow

instance TypelevelMIME Woff where
  fromValue MIME.Woff = Just Woff
  fromValue _ = Nothing
  value = MIME.Woff

data Woff2 = Woff2

derive instance Generic Woff2 _
derive instance Eq Woff2
instance Show Woff2 where
  show = genericShow

instance TypelevelMIME Woff2 where
  fromValue MIME.Woff2 = Just Woff2
  fromValue _ = Nothing
  value = MIME.Woff2

data Xhtml = Xhtml

derive instance Generic Xhtml _
derive instance Eq Xhtml
instance Show Xhtml where
  show = genericShow

instance TypelevelMIME Xhtml where
  fromValue MIME.Xhtml = Just Xhtml
  fromValue _ = Nothing
  value = MIME.Xhtml

data Xls = Xls

derive instance Generic Xls _
derive instance Eq Xls
instance Show Xls where
  show = genericShow

instance TypelevelMIME Xls where
  fromValue MIME.Xls = Just Xls
  fromValue _ = Nothing
  value = MIME.Xls

data Xlsx = Xlsx

derive instance Generic Xlsx _
derive instance Eq Xlsx
instance Show Xlsx where
  show = genericShow

instance TypelevelMIME Xlsx where
  fromValue MIME.Xlsx = Just Xlsx
  fromValue _ = Nothing
  value = MIME.Xlsx

data Xml = Xml

derive instance Generic Xml _
derive instance Eq Xml
instance Show Xml where
  show = genericShow

instance TypelevelMIME Xml where
  fromValue MIME.Xml = Just Xml
  fromValue _ = Nothing
  value = MIME.Xml

data Xul = Xul

derive instance Generic Xul _
derive instance Eq Xul
instance Show Xul where
  show = genericShow

instance TypelevelMIME Xul where
  fromValue MIME.Xul = Just Xul
  fromValue _ = Nothing
  value = MIME.Xul

data Zip = Zip

derive instance Generic Zip _
derive instance Eq Zip
instance Show Zip where
  show = genericShow

instance TypelevelMIME Zip where
  fromValue MIME.Zip = Just Zip
  fromValue _ = Nothing
  value = MIME.Zip

data Video3gp = Video3gp

derive instance Generic Video3gp _
derive instance Eq Video3gp
instance Show Video3gp where
  show = genericShow

instance TypelevelMIME Video3gp where
  fromValue MIME.Video3gp = Just Video3gp
  fromValue _ = Nothing
  value = MIME.Video3gp

data Video3g2 = Video3g2

derive instance Generic Video3g2 _
derive instance Eq Video3g2
instance Show Video3g2 where
  show = genericShow

instance TypelevelMIME Video3g2 where
  fromValue MIME.Video3g2 = Just Video3g2
  fromValue _ = Nothing
  value = MIME.Video3g2

data Archive7z = Archive7z

derive instance Generic Archive7z _
derive instance Eq Archive7z
instance Show Archive7z where
  show = genericShow

instance TypelevelMIME Archive7z where
  fromValue MIME.Archive7z = Just Archive7z
  fromValue _ = Nothing
  value = MIME.Archive7z
