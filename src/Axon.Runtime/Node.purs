module Axon.Runtime.Node where

import Prelude

import Axon.Request (Body(..), Request)
import Axon.Request as Request
import Axon.Request.Method as Method
import Axon.Response (Response)
import Axon.Response as Rep
import Axon.Runtime (class Runtime)
import Control.Monad.Error.Class (liftMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Net.SocketAddress (SocketAddress)
import Data.Net.SocketAddress as SocketAddress
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Null
import Data.String.Lower as String.Lower
import Data.Tuple.Nested (type (/\))
import Data.URL (URL)
import Data.URL as URL
import Effect (Effect)
import Effect.Aff (launchAff_, makeAff)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, UnliftAff(..), askUnliftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.EventEmitter (EventHandle(..))
import Node.EventEmitter as Event
import Node.EventEmitter.UtilTypes (EventHandle0)
import Node.Stream (Readable)

foreign import data Server :: Type
foreign import data IncomingMessage :: Type
foreign import data ServerResponse :: Type

type CreateServer =
  { keepAliveTimeout :: Nullable Number
  , fetch :: IncomingMessage -> ServerResponse -> Effect Unit
  }

foreign import createServer :: CreateServer -> Effect Server
foreign import serverClose :: Effect Unit -> Server -> Effect Unit
foreign import serverListen ::
  Effect Unit -> String -> Int -> Server -> Effect Unit

foreign import requestBody :: IncomingMessage -> Readable ()
foreign import requestHeaders ::
  IncomingMessage -> Effect (Object (Array String))

foreign import requestMethod :: IncomingMessage -> Effect String
foreign import requestURL :: IncomingMessage -> Effect String
foreign import requestAddr ::
  { ipv4 :: String -> Int -> SocketAddress
  , ipv6 :: String -> Int -> SocketAddress
  } ->
  IncomingMessage ->
  Effect SocketAddress

foreign import responseEnd :: ServerResponse -> Effect Unit
foreign import responseWriteHead ::
  Int -> Object (Array String) -> ServerResponse -> Effect Unit

foreign import responseWriteString :: String -> ServerResponse -> Effect Unit
foreign import responseWriteBuffer :: Buffer -> ServerResponse -> Effect Unit
foreign import responseWriteStream ::
  Readable () -> ServerResponse -> Effect Unit

closeH :: EventHandle0 Server
closeH = EventHandle "close" identity

toRequest :: URL -> IncomingMessage -> Effect Request
toRequest baseURI req = do
  let
    body = BodyReadable $ requestBody req
  headers <-
    requestHeaders req
      <#>
        ( Map.fromFoldable
            <<< (Object.toUnfoldable :: _ (Array (String /\ Array String)))
        )
  url <- requestURL req <#> URL.addSegment baseURI
  method <- do
    methodString <- requestMethod req
    liftMaybe (error $ "unknown request method: " <> methodString)
      $ Method.fromString methodString
  address <- requestAddr { ipv4: SocketAddress.IPv4, ipv6: SocketAddress.IPv6 }
    req

  Request.make { address, body, headers, url, method }

writeResponse :: ServerResponse -> Response -> Effect Unit
writeResponse node rep = do
  responseWriteHead
    (unwrap $ Rep.status rep)
    ( Rep.headers rep
        #
          ( Map.toUnfoldable ::
              _ (Array (String.Lower.StringLower /\ Array String))
          )
        <#> lmap String.Lower.toString
        # Object.fromFoldable
    )
    node

  case Rep.body rep of
    Rep.BodyEmpty -> pure unit
    Rep.BodyBuffer b -> responseWriteBuffer b node
    Rep.BodyString s -> responseWriteString s node
    Rep.BodyReadable r -> responseWriteStream r node

  responseEnd node

fetchImpl ::
  forall m.
  MonadUnliftAff m =>
  String ->
  (Request -> m Response) ->
  m (IncomingMessage -> ServerResponse -> Effect Unit)
fetchImpl hostname f = do
  baseURI <- liftAff $ liftMaybe (error $ "invalid hostname " <> hostname)
    $ URL.fromString
    $ "http://"
    <> hostname
  UnliftAff toAff <- askUnliftAff
  pure \req' rep' ->
    launchAff_
      $ toAff
      $ liftEffect (toRequest baseURI req')
      >>= f
      >>= (liftEffect <<< writeResponse rep')

instance Runtime Server where
  serve o = do
    fetch <- fetchImpl o.hostname o.fetch
    server <- liftEffect $ createServer
      { keepAliveTimeout: Null.notNull $ unwrap o.idleTimeout, fetch }
    liftAff
      $ makeAff \res ->
          serverListen
            (res $ Right unit)
            o.hostname
            o.port
            server
            $> mempty
    join' <-
      liftAff
      $ Aff.forkAff
      $ Aff.makeAff
      $ \res -> Event.on closeH (res $ Right unit) server $> mempty

    pure
      { server
      , join: join'
      , stop: liftAff $ makeAff \res -> serverClose (res $ Right unit) server $>
          mempty
      }
