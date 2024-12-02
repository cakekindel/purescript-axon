module Axon.Request.Parts.Class
  ( class RequestParts
  , extractRequestParts
  , Header(..)
  , module Parts.Method
  , module Parts.Body
  , module Path.Parts
  ) where

import Prelude

import Axon.Header.Typed (class TypedHeader, headerName, headerValueParser)
import Axon.Request (Request)
import Axon.Request as Request
import Axon.Request.Method (Method)
import Axon.Request.Method as Method
import Axon.Request.Parts.Body (Json(..), Stream(..))
import Axon.Request.Parts.Body (Json(..), Stream(..)) as Parts.Body
import Axon.Request.Parts.Method (Connect(..), Delete(..), Get(..), Options(..), Patch(..), Post(..), Put(..), Trace(..))
import Axon.Request.Parts.Method (Get(..), Post(..), Put(..), Patch(..), Delete(..), Trace(..), Options(..), Connect(..)) as Parts.Method
import Axon.Request.Parts.Path (Path(..)) as Path.Parts
import Axon.Request.Parts.Path (class DiscardTupledUnits, class PathParts, Path(..), discardTupledUnits, extractPathParts)
import Axon.Response (Response)
import Axon.Response as Response
import Control.Alternative (guard)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), hush)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.String.Lower as String.Lower
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL as URL
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Parsing (runParser)

newtype Header a = Header a
derive instance Generic (Header a) _
derive instance Newtype (Header a) _
derive newtype instance (Eq a) => Eq (Header a)
derive newtype instance (Ord a) => Ord (Header a)
derive newtype instance (Show a) => Show (Header a)

extractMethod ::
  forall a.
  a ->
  Method ->
  Request ->
  Aff (Either Response (Maybe a))
extractMethod a method r =
  if Request.method r == method then
    pure $ Right $ Just a
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

instance RequestParts (Either Request.BodyStringError String) where
  extractRequestParts r =
    Request.bodyString r
      <#> Just
      <#> Right

instance TypedHeader a => RequestParts (Header a) where
  extractRequestParts r = runExceptT $ runMaybeT do
    value <- Request.headers r # Map.lookup (String.Lower.fromString $ headerName @a) # pure # MaybeT
    runParser value (headerValueParser @a) # hush # pure # MaybeT <#> Header

instance (PathParts a b, DiscardTupledUnits b c) => RequestParts (Path a c) where
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
        <#> discardTupledUnits
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

instance RequestParts Get where
  extractRequestParts = extractMethod Get Method.GET

instance RequestParts Post where
  extractRequestParts = extractMethod Post Method.POST

instance RequestParts Put where
  extractRequestParts = extractMethod Put Method.PUT

instance RequestParts Patch where
  extractRequestParts = extractMethod Patch Method.PATCH

instance RequestParts Delete where
  extractRequestParts = extractMethod Delete Method.DELETE

instance RequestParts Options where
  extractRequestParts = extractMethod Options Method.OPTIONS

instance RequestParts Connect where
  extractRequestParts = extractMethod Connect Method.CONNECT

instance RequestParts Trace where
  extractRequestParts = extractMethod Trace Method.TRACE

instance (RequestParts a, RequestParts b) => RequestParts (a /\ b) where
  extractRequestParts r = runExceptT $ runMaybeT do
    a <- extractRequestParts @a r # ExceptT # MaybeT
    b <- extractRequestParts @b r # ExceptT # MaybeT
    pure $ a /\ b
