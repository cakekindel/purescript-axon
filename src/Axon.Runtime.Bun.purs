module Axon.Runtime.Bun where

import Prelude

import Axon.Request (Request)
import Axon.Response (Response)
import Axon.Runtime (class Runtime)
import Axon.Web.Request (WebRequest)
import Axon.Web.Request as WebRequest
import Axon.Web.Response (WebResponse)
import Axon.Web.Response as WebResponse
import Control.Monad.Error.Class (try)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.Nullable as Null
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Node.Net.Types (IPv4, IPv6, SocketAddress)

foreign import data Bun :: Type

type Serve =
  { port :: Nullable Int
  , hostname :: Nullable String
  , idleTimeout :: Nullable Number
  , fetch :: WebRequest -> Bun -> Effect (Promise WebResponse)
  }

foreign import serve :: Serve -> Effect Bun
foreign import stop :: Bun -> Promise Unit
foreign import ref :: Bun -> Effect Unit
foreign import unref :: Bun -> Effect Unit
foreign import requestAddr ::
  { left :: forall a b. a -> Either a b, right :: forall a b. b -> Either a b } ->
  WebRequest ->
  Bun ->
  Effect (Either (SocketAddress IPv4) (SocketAddress IPv6))

fetchImpl ::
  (Request -> Aff Response) -> WebRequest -> Bun -> Effect (Promise WebResponse)
fetchImpl f req bun =
  Promise.fromAff do
    addr <- liftEffect $ requestAddr { left: Left, right: Right } req bun
    req' <- liftEffect $ WebRequest.toRequest addr req
    f req' >>= (liftEffect <<< WebResponse.fromResponse)

instance Runtime Bun where
  serve o = do
    -- Killing `stopSignal` causes `stopFiber` to complete
    stopSignal <- Aff.forkAff Aff.never
    stopFiber <- Aff.forkAff $ void $ try $ Aff.joinFiber stopSignal

    let
      o' =
        { port: Null.toNullable o.port
        , hostname: Null.toNullable o.hostname
        , idleTimeout: Null.toNullable $ unwrap <$> o.idleTimeout
        , fetch: fetchImpl o.fetch
        }

    bun <- liftEffect $ serve o'
    liftEffect $ ref bun

    pure
      { server: bun
      , join: stopFiber
      , stop: do
          Promise.toAff $ stop bun
          Aff.killFiber (error "") stopSignal
      }
