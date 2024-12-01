module Axon.Response.Body where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Effect (Effect)
import Effect.Aff.HTTP.Form (Form, RawFormData) as HTTP
import Effect.Aff.HTTP.Form as HTTP.Form
import Node.Buffer (Buffer)
import Node.Stream as Stream

data Body
  = BodyEmpty
  | BodyString String
  | BodyBuffer Buffer
  | BodyFormData HTTP.RawFormData
  | BodyReadable (Stream.Readable ())

instance Show Body where
  show BodyEmpty = "BodyEmpty"
  show (BodyString s) = "BodyString " <> show s
  show (BodyBuffer _) = "BodyBuffer _"
  show (BodyFormData _) = "BodyFormData _"
  show (BodyReadable _) = "BodyReadable _"

formBody :: HTTP.Form -> Effect Body
formBody f = HTTP.Form.toRawFormData f <#> BodyFormData

stringBody :: String -> Body
stringBody = BodyString

bufferBody :: Buffer -> Body
bufferBody = BodyBuffer

streamBody :: Stream.Readable () -> Body
streamBody = BodyReadable

emptyBody :: Body
emptyBody = BodyEmpty

jsonBody :: Json -> Body
jsonBody = stringify >>> BodyString
