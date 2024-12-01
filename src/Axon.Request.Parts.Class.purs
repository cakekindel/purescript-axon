module Axon.Request.Parts.Class (class RequestParts, extractRequestParts, module Parts.Method, module Parts.Body, module Path.Parts) where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL as URL
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Axon.Request (Request)
import Axon.Request as Request
import Axon.Request.Method (Method)
import Axon.Request.Method as Method
import Axon.Request.Parts.Body (Json(..), Stream(..))
import Axon.Request.Parts.Body (Json(..), Stream(..)) as Parts.Body
import Axon.Request.Parts.Method (Connect, Delete, Get, Options, Patch, Post, Put, Trace)
import Axon.Request.Parts.Method (Get(..), Post(..), Put(..), Patch(..), Delete(..), Trace(..), Options(..), Connect(..)) as Parts.Method
import Axon.Request.Parts.Path (Path(..)) as Path.Parts
import Axon.Request.Parts.Path (class PathParts, Path(..), extractPathParts)
import Axon.Response (Response)
import Axon.Response as Response

extractMethod :: forall @t a. RequestParts a => Newtype t a => Method -> Request -> Aff (Either Response (Maybe t))
extractMethod method r =
  if Request.method r == method then
    extractRequestParts @a r
      # ExceptT
      # MaybeT
      <#> wrap
      # runMaybeT
      # runExceptT
  else
    pure $ Right Nothing

class RequestParts a where
  extractRequestParts :: Request -> Aff (Either Response (Maybe a))

instance RequestParts Unit where
  extractRequestParts _ = pure unit # runMaybeT # runExceptT

instance RequestParts Request where
  extractRequestParts r = pure r # runMaybeT # runExceptT

instance RequestParts String where
  extractRequestParts r =
    Request.bodyString r
    <#> lmap (const $ Response.fromStatus 500)
    # ExceptT
    # lift
    # runMaybeT
    # runExceptT

instance PathParts a b => RequestParts (Path a b) where
  extractRequestParts r =
    let
      segments = Request.url r # URL.path # case _ of
        URL.PathAbsolute a -> a
        URL.PathRelative a -> a
        _ -> []
      extract = extractPathParts @a @b (Request.url r)
      ensureConsumed (leftover /\ x) = guard (Array.null leftover) $> x
    in
      segments
        # extract
        # Right
        # MaybeT
        >>= ensureConsumed
        <#> Path
        # runMaybeT
        # pure

instance (DecodeJson a) => RequestParts (Json a) where
  extractRequestParts r =
    let
      jsonBody =
        Request.bodyJSON r
          <#> lmap (const $ Response.fromStatus 500)
          # ExceptT
          # lift
      decode j =
        decodeJson j
          # lmap (const $ Response.fromStatus 400)
          # pure
          # ExceptT
          # lift
    in
      jsonBody >>= decode <#> Json # runMaybeT # runExceptT

instance RequestParts Buffer where
  extractRequestParts r =
    let
      bufBody =
        Request.bodyBuffer r
          <#> lmap (const $ Response.fromStatus 500)
          # ExceptT
          # lift
    in
      bufBody # runMaybeT # runExceptT

instance RequestParts Stream where
  extractRequestParts r =
    let
      streamBody =
        Request.bodyReadable r
          <#> lmap (const $ Response.fromStatus 500)
          # ExceptT
          # lift
    in
      streamBody <#> Stream # runMaybeT # runExceptT # liftEffect

instance (RequestParts a) => RequestParts (Get a) where
  extractRequestParts = extractMethod @(Get a) Method.GET

instance (RequestParts a) => RequestParts (Post a) where
  extractRequestParts = extractMethod @(Post a) Method.POST

instance (RequestParts a) => RequestParts (Put a) where
  extractRequestParts = extractMethod @(Put a) Method.PUT

instance (RequestParts a) => RequestParts (Patch a) where
  extractRequestParts = extractMethod @(Patch a) Method.PATCH

instance (RequestParts a) => RequestParts (Delete a) where
  extractRequestParts = extractMethod @(Delete a) Method.DELETE

instance (RequestParts a) => RequestParts (Options a) where
  extractRequestParts = extractMethod @(Options a) Method.OPTIONS

instance (RequestParts a) => RequestParts (Connect a) where
  extractRequestParts = extractMethod @(Connect a) Method.CONNECT

instance (RequestParts a) => RequestParts (Trace a) where
  extractRequestParts = extractMethod @(Trace a) Method.TRACE

instance (RequestParts a, RequestParts b) => RequestParts (a /\ b) where
  extractRequestParts r = runExceptT $ runMaybeT do
    a <- extractRequestParts @a r # ExceptT # MaybeT
    b <- extractRequestParts @b r # ExceptT # MaybeT
    pure $ a /\ b
