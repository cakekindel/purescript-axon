module Axon.Request.Handler where

import Prelude

import Axon.Request (Request)
import Axon.Request.Parts.Class
  ( class RequestParts
  , ExtractError(..)
  , extractRequestParts
  )
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff, liftAff)

newtype Handler m a = Handler (Request -> m (Either ExtractError a))

derive instance Newtype (Handler m a) _

or ::
  forall m a b r.
  RequestHandler a m r =>
  RequestHandler b m r =>
  a ->
  b ->
  Handler m r
or a b = toHandler a <> toHandler b

instance Monad m => Semigroup (Handler m a) where
  append (Handler a) (Handler b) = Handler \req ->
    a req >>= case _ of
      Left ExtractNext -> b req
      a' -> pure a'

class MonadAff m <= RequestHandler a m b | a -> b m where
  toHandler :: a -> Handler m b
  invokeHandler :: a -> Request -> m (Either ExtractError b)

instance (MonadAff m) => RequestHandler (Handler m b) m b where
  toHandler = identity
  invokeHandler (Handler f) = f
else instance
  ( MonadAff m
  , RequestHandler f m b
  , RequestParts a
  ) =>
  RequestHandler (a -> f) m b where
  toHandler f = Handler $ invokeHandler f
  invokeHandler f req = runExceptT do
    a <- ExceptT $ liftAff $ extractRequestParts @a req
    ExceptT $ invokeHandler (f a) req
else instance (MonadAff m) => RequestHandler (m a) m a where
  toHandler m = Handler $ const $ m <#> Right
  invokeHandler m _ = m <#> Right
