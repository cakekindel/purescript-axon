module Axon.Runtime (Init, Handle, class Runtime, serve) where

import Prelude

import Axon.Request (Request)
import Axon.Response (Response)
import Control.Monad.Fork.Class (class MonadFork)
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds)
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)

type Init m =
  { fetch :: Request -> m Response
  , port :: Maybe Int
  , hostname :: Maybe String
  , idleTimeout :: Maybe Seconds
  }

type Handle m f a =
  { server :: a
  , join :: f Unit
  , stop :: m Unit
  }

class Runtime :: Type -> Constraint
class Runtime a where
  serve ::
    forall m f. MonadFork f m => MonadUnliftAff m => Init m -> m (Handle m f a)
