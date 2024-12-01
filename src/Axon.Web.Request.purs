module Axon.Web.Request where

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Nullable (Nullable)
import Effect (Effect)
import Node.Stream as Stream
import Axon.Request.Web (WebHeaders)
import Web.Streams.ReadableStream (ReadableStream)

foreign import data WebRequest :: Type

foreign import body :: WebRequest -> Effect (Nullable (ReadableStream Uint8Array))
foreign import bodyUsed :: WebRequest -> Effect Boolean
foreign import method :: WebRequest -> Effect String
foreign import url :: WebRequest -> Effect String

foreign import headers :: WebRequest -> Effect WebHeaders

foreign import readableFromWeb :: ReadableStream Uint8Array -> Effect (Stream.Readable ())
