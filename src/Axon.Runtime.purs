module Axon.Runtime (Init, Handle, class Runtime, serve) where

import Prelude

import Axon.Request (Request)
import Axon.Response (Response)
import Data.Time.Duration (Seconds)
import Effect.Aff (Fiber)
import Effect.Aff.Unlift (class MonadUnliftAff)

type Init m =
  { fetch :: Request -> m Response
  , port :: Int
  , hostname :: String
  , idleTimeout :: Seconds
  }

type Handle m a =
  { server :: a
  , join :: Fiber Unit
  , stop :: m Unit
  }

class Runtime :: Type -> Constraint
class Runtime a where
  serve :: forall m. MonadUnliftAff m => Init m -> m (Handle m a)
