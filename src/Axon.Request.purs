module Axon.Request (Request, Body(..), BodyReadableError(..), BodyStringError(..), BodyJSONError(..), BodyBufferError(..), bodyReadable, bodyString, bodyJSON, bodyBuffer, headers, method, address, url, contentType, accept, contentLength, lookupHeader, make) where

import Prelude

import Axon.Request.Method (Method)
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (stringify) as JSON
import Data.Argonaut.Parser (jsonParser) as JSON
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.MIME (MIME)
import Data.MIME as MIME
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String.Lower (StringLower)
import Data.String.Lower as String.Lower
import Data.URL (URL)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Exception as Error
import Effect.Ref (Ref) as Effect
import Effect.Ref as Ref
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Net.Types (IPv4, IPv6, SocketAddress)
import Node.Stream as Stream
import Node.Stream.Aff as Stream.Aff

data BodyReadableError
  = BodyReadableErrorHasBeenConsumed
  | BodyReadableErrorEmpty

derive instance Generic BodyReadableError _
derive instance Eq BodyReadableError
instance Show BodyReadableError where
  show = genericShow

data BodyBufferError
  = BodyBufferErrorReadable BodyReadableError
  | BodyBufferErrorReading Error

derive instance Generic BodyBufferError _
instance Eq BodyBufferError where
  eq (BodyBufferErrorReadable a) (BodyBufferErrorReadable b) = a == b
  eq (BodyBufferErrorReading a) (BodyBufferErrorReading b) = Error.message a == Error.message b
  eq _ _ = false

instance Show BodyBufferError where
  show = genericShow

data BodyStringError
  = BodyStringErrorBuffer BodyBufferError
  | BodyStringErrorNotUTF8

derive instance Generic BodyStringError _
derive instance Eq BodyStringError
instance Show BodyStringError where
  show = genericShow

data BodyJSONError
  = BodyJSONErrorString BodyStringError
  | BodyJSONErrorParsing String

derive instance Generic BodyJSONError _
derive instance Eq BodyJSONError
instance Show BodyJSONError where
  show = genericShow

data Body
  = BodyEmpty
  | BodyReadable (Stream.Readable ())
  | BodyReadableConsumed
  | BodyCached Buffer
  | BodyCachedString String
  | BodyCachedJSON Json

data Request =
  Request
    { headers :: Map StringLower String
    , address :: Either (SocketAddress IPv4) (SocketAddress IPv6)
    , url :: URL
    , method :: Method
    , bodyRef :: Effect.Ref Body
    }

make
  :: { headers :: Map String String
     , address :: Either (SocketAddress IPv4) (SocketAddress IPv6)
     , url :: URL
     , method :: Method
     , body :: Body
     }
  -> Effect Request
make a = do
  bodyRef <- Ref.new a.body
  pure $ Request { bodyRef: bodyRef, headers: foldlWithIndex (\k m v -> Map.insert (String.Lower.fromString k) v m) Map.empty a.headers, address: a.address, url: a.url, method: a.method }

headers :: Request -> Map StringLower String
headers (Request a) = a.headers

lookupHeader :: String -> Request -> Maybe String
lookupHeader k (Request a) = Map.lookup (String.Lower.fromString k) a.headers

contentType :: Request -> Maybe MIME
contentType = lookupHeader "content-type" >>> map MIME.fromString

accept :: Request -> Maybe MIME
accept = lookupHeader "accept" >>> map MIME.fromString

contentLength :: Request -> Maybe Int
contentLength = lookupHeader "content-length" >=> Int.fromString

method :: Request -> Method
method (Request a) = a.method

address :: Request -> Either (SocketAddress IPv4) (SocketAddress IPv6)
address (Request a) = a.address

url :: Request -> URL
url (Request a) = a.url

bodyReadable :: Request -> Effect (Either BodyReadableError (Stream.Readable ()))
bodyReadable (Request { bodyRef }) = runExceptT do
  body <- liftEffect $ Ref.read bodyRef
  case body of
    BodyEmpty -> throwError BodyReadableErrorEmpty
    BodyReadableConsumed -> throwError BodyReadableErrorHasBeenConsumed
    BodyReadable r ->
      Ref.write BodyReadableConsumed bodyRef $> r # lift
    BodyCached buf -> Stream.readableFromBuffer buf # lift
    BodyCachedString str -> Stream.readableFromString str UTF8 # lift
    BodyCachedJSON json -> json # JSON.stringify # flip Buffer.fromString UTF8 >>= Stream.readableFromBuffer # lift

bodyBuffer :: Request -> Aff (Either BodyBufferError Buffer)
bodyBuffer r@(Request { bodyRef }) =
  let
    stream =
      bodyReadable r
        # liftEffect
        <#> lmap BodyBufferErrorReadable
        # ExceptT
    readAll s =
      Stream.Aff.readAll s
        # liftAff
        # try
        <#> lmap BodyBufferErrorReading
        # ExceptT
        >>= (liftEffect <<< Buffer.concat)
  in
    runExceptT do
      body <- Ref.read bodyRef # liftEffect
      case body of
        BodyCached buf -> pure buf
        BodyCachedString str -> Buffer.fromString str UTF8 # liftEffect
        BodyCachedJSON json -> Buffer.fromString (JSON.stringify json) UTF8 # liftEffect
        _ -> do
          buf <- stream >>= readAll
          Ref.write (BodyCached buf) bodyRef $> buf # liftEffect

bodyString :: Request -> Aff (Either BodyStringError String)
bodyString r@(Request { bodyRef }) =
  let
    buf =
      bodyBuffer r
        <#> lmap BodyStringErrorBuffer
        # ExceptT
    bufString b =
      Buffer.toString UTF8 b
        # liftEffect
        # try
        <#> lmap (const BodyStringErrorNotUTF8)
        # ExceptT
  in
    runExceptT do
      body <- Ref.read bodyRef # liftEffect
      case body of
        BodyCachedString str -> pure str
        BodyCachedJSON json -> JSON.stringify json # pure
        _ -> do
          str <- buf >>= bufString
          Ref.write (BodyCachedString str) bodyRef $> str # liftEffect

bodyJSON :: Request -> Aff (Either BodyJSONError Json)
bodyJSON r@(Request { bodyRef }) =
  let
    str =
      bodyString r
        <#> lmap BodyJSONErrorString
        # ExceptT
    parse s =
      JSON.jsonParser s
        # lmap BodyJSONErrorParsing
        # pure
        # ExceptT
  in
    runExceptT do
      body <- Ref.read bodyRef # liftEffect
      case body of
        BodyCachedJSON j -> pure j
        _ -> do
          j <- str >>= parse
          Ref.write (BodyCachedJSON j) bodyRef $> j # liftEffect
