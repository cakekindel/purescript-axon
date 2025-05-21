module Axon.Web.Request where

import Prelude

import Axon.Request (Request)
import Axon.Request as Request
import Axon.Request.Method as Method
import Axon.Web.Headers (WebHeaders)
import Axon.Web.Headers as WebHeaders
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Maybe (fromMaybe)
import Data.Net.SocketAddress (SocketAddress)
import Data.Nullable (Nullable)
import Data.Nullable as Null
import Data.URL as URL
import Effect (Effect)
import Effect.Exception (error)
import Node.Stream as Stream
import Web.Streams.ReadableStream (ReadableStream)

foreign import data WebRequest :: Type

foreign import body ::
  WebRequest -> Effect (Nullable (ReadableStream Uint8Array))

foreign import bodyUsed :: WebRequest -> Effect Boolean
foreign import method :: WebRequest -> Effect String
foreign import url :: WebRequest -> Effect String

foreign import headers :: WebRequest -> Effect WebHeaders

foreign import readableFromWeb ::
  ReadableStream Uint8Array -> Effect (Stream.Readable ())

toRequest ::
  SocketAddress ->
  WebRequest ->
  Effect Request
toRequest address req =
  let
    body' =
      fromMaybe Request.BodyEmpty <$> runMaybeT do
        readable <- MaybeT $ Null.toMaybe <$> body req
        lift $ Request.BodyReadable <$> readableFromWeb readable
    headers' = headers req >>= WebHeaders.toMap
    url' = do
      urlString <- url req
      liftMaybe (error $ "invalid URL: " <> urlString) $ URL.fromString
        urlString
    method' = do
      methodString <- method req
      liftMaybe (error $ "unknown request method: " <> methodString) $
        Method.fromString methodString
  in
    join
      $ pure
          ( \b h u m -> Request.make
              { body: b, headers: h, address, url: u, method: m }
          )
      <*> body'
      <*> headers'
      <*> url'
      <*> method'
