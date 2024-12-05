module Axon.Response.Body where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Effect (Effect)
import Node.Buffer (Buffer)
import Node.Stream as Stream

data Body
  = BodyEmpty
  | BodyString String
  | BodyBuffer Buffer
  | BodyReadable (Stream.Readable ())

instance Show Body where
  show BodyEmpty = "BodyEmpty"
  show (BodyString s) = "BodyString " <> show s
  show (BodyBuffer _) = "BodyBuffer _"
  show (BodyReadable _) = "BodyReadable _"

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
