module Axon.Runtime (Init, Handle, class Runtime, serve) where

import Prelude

import Axon.Request (Request)
import Axon.Response (Response)
import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds)
import Effect (Effect)
import Effect.Aff (Aff, Fiber)

type Init =
  { fetch :: Request -> Aff Response
  , port :: Maybe Int
  , hostname :: Maybe String
  , idleTimeout :: Maybe Seconds
  }

type Handle a =
  { server :: a
  , join :: Fiber Unit
  , stop :: Aff Unit
  }

class Runtime :: Type -> Constraint
class Runtime a where
  serve :: Init -> Aff (Handle a)
