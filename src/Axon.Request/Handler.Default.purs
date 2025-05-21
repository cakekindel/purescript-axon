module Axon.Request.Handler.Default where

import Prelude

import Axon.Request.Handler (Handler(..))
import Axon.Response (Response)
import Axon.Response as Response
import Axon.Response.Status as Status
import Data.Either (Either(..))
import Effect.Aff.Class (class MonadAff)

notFound :: forall m. MonadAff m => Handler m Response
notFound = Handler $ const $ pure $ Right $ Response.fromStatus $
  Status.notFound
