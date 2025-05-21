module Axon.Runtime.Bun where

import Prelude hiding (join)

import Axon.Request (Request)
import Axon.Response (Response)
import Axon.Runtime (class Runtime)
import Axon.Web.Request (WebRequest)
import Axon.Web.Request as WebRequest
import Axon.Web.Response (WebResponse)
import Axon.Web.Response as WebResponse
import Control.Monad.Error.Class (try)
import Control.Monad.Fork.Class (fork)
import Promise.Aff (Promise)
import Promise.Aff as Promise
import Data.Net.SocketAddress (SocketAddress)
import Data.Net.SocketAddress as SocketAddress
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, UnliftAff(..), askUnliftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)

foreign import data Bun :: Type

type Serve =
  { port :: Int
  , hostname :: String
  , idleTimeout :: Number
  , fetch :: WebRequest -> Bun -> Effect (Promise WebResponse)
  }

foreign import serve :: Serve -> Effect Bun
foreign import stop :: Bun -> Promise Unit
foreign import ref :: Bun -> Effect Unit
foreign import unref :: Bun -> Effect Unit
foreign import requestAddr ::
  { ipv4 :: String -> Int -> SocketAddress
  , ipv6 :: String -> Int -> SocketAddress
  } ->
  WebRequest ->
  Bun ->
  Effect SocketAddress

fetchImpl ::
  forall m.
  MonadUnliftAff m =>
  (Request -> m Response) ->
  m
    ( WebRequest ->
      Bun ->
      Effect (Promise WebResponse)
    )
fetchImpl f = do
  UnliftAff toAff <- askUnliftAff
  pure \req bun ->
    Promise.fromAff do
      addr <- liftEffect $ requestAddr
        { ipv4: SocketAddress.IPv4, ipv6: SocketAddress.IPv6 }
        req
        bun
      req' <- liftEffect $ WebRequest.toRequest addr req
      toAff $ f req' >>= (liftEffect <<< WebResponse.fromResponse)

instance Runtime Bun where
  serve o = do
    -- `stopSignal` will never resolve, but it can be killed.
    stopSignal <- liftAff $ Aff.forkAff Aff.never

    -- blocks on `stopSignal`, resolving when it's killed.
    stopFiber <- fork $ liftAff $ void $ try $ Aff.joinFiber stopSignal

    fetch <- fetchImpl o.fetch

    let
      o' =
        { port: o.port
        , hostname: o.hostname
        , idleTimeout: unwrap o.idleTimeout
        , fetch
        }

    bun <- liftEffect $ serve o'
    liftEffect $ ref bun

    pure
      { server: bun
      , join: stopFiber
      , stop: liftAff do
          Promise.toAff $ stop bun
          Aff.killFiber (error "") stopSignal
      }
