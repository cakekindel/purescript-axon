module Axon.Request.Parts.Class
  ( class RequestParts
  , extractRequestParts
  , Try(..)
  , tryEither
  , tryToEither
  , ExtractError(..)
  , module Parts.Header
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
import Axon.Request.Parts.Header (Header(..), HeaderMap(..))
import Axon.Request.Parts.Header (Header(..), HeaderMap(..)) as Parts.Header
import Axon.Request.Parts.Method
  ( Connect(..)
  , Delete(..)
  , Get(..)
  , Options(..)
  , Patch(..)
  , Post(..)
  , Put(..)
  , Trace(..)
  )
import Axon.Request.Parts.Method
  ( Get(..)
  , Post(..)
  , Put(..)
  , Patch(..)
  , Delete(..)
  , Trace(..)
  , Options(..)
  , Connect(..)
  ) as Parts.Method
import Axon.Request.Parts.Path (Path(..)) as Path.Parts
import Axon.Request.Parts.Path
  ( class DiscardTupledUnits
  , class PathParts
  , Path(..)
  , discardTupledUnits
  , extractPathParts
  )
import Control.Monad.Error.Class (liftEither, liftMaybe, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), hush, note)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String.Lower as String.Lower
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL as URL
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Parsing (runParser)
import Parsing.String (parseErrorHuman)

data Try err ok = Err err | Ok ok

derive instance Generic (Try a b) _
derive instance (Eq a, Eq b) => Eq (Try a b)

instance (Show a, Show b) => Show (Try a b) where
  show = genericShow

tryEither :: forall a b. Either a b -> Try a b
tryEither (Left a) = Err a
tryEither (Right b) = Ok b

tryToEither :: forall a b. Try a b -> Either a b
tryToEither (Err a) = Left a
tryToEither (Ok b) = Right b

data ExtractError
  = ExtractError String
  | ExtractNext
  | ExtractBadRequest String

derive instance Generic ExtractError _
derive instance Eq ExtractError
instance Show ExtractError where
  show = genericShow

extractMethod ::
  forall a.
  a ->
  Method ->
  Request ->
  Aff (Either ExtractError a)
extractMethod a method r = runExceptT do
  when (Request.method r /= method) $ throwError ExtractNext
  pure a

class RequestParts a where
  extractRequestParts :: Request -> Aff (Either ExtractError a)

instance RequestParts Unit where
  extractRequestParts _ = pure unit # runExceptT

instance RequestParts Request where
  extractRequestParts r = pure r # runExceptT

instance RequestParts String where
  extractRequestParts r =
    Request.bodyString r
      <#> lmap (const $ ExtractBadRequest "Expected body to be valid UTF-8")

instance RequestParts (Try Request.BodyStringError String) where
  extractRequestParts r = Request.bodyString r <#> tryEither <#> Right

instance (RequestParts a, RequestParts b) => RequestParts (Either a b) where
  extractRequestParts r =
    extractRequestParts @a r
      >>=
        case _ of
          Right a' -> pure $ Right $ Left a'
          _ -> extractRequestParts @b r <#> map Right

instance (RequestParts a) => RequestParts (Maybe a) where
  extractRequestParts r = extractRequestParts @a r <#> hush <#> Right

instance TypedHeader a => RequestParts (Header a) where
  extractRequestParts r = runExceptT do
    value <-
      Request.headers r
        # Map.lookup (String.Lower.fromString $ headerName @a)
        >>= Array.head
        # liftMaybe ExtractNext
    runParser value (headerValueParser @a)
      # bimap
          ( ExtractBadRequest
              <<< Array.intercalate "\n"
              <<< parseErrorHuman value 5
          )
          Header
      # liftEither

instance RequestParts HeaderMap where
  extractRequestParts r = runExceptT $ pure $ HeaderMap $ Request.headers r

instance (PathParts a b, DiscardTupledUnits b c) => RequestParts (Path a c) where
  extractRequestParts r =
    let
      segments = Request.url r # URL.path # case _ of
        URL.PathAbsolute a -> a
        URL.PathRelative a -> a
        _ -> []
      extract = extractPathParts @a @b (Request.url r)
      ensureConsumed (leftover /\ x) =
        when (not $ Array.null leftover) (throwError ExtractNext) $> x
    in
      segments
        # extract
        # note ExtractNext
        >>= ensureConsumed
        <#> discardTupledUnits
        <#> Path
        # pure

instance (DecodeJson a) => RequestParts (Json a) where
  extractRequestParts r =
    let
      jsonBody =
        Request.bodyJSON r
          <#> lmap (ExtractBadRequest <<< show)
          # ExceptT
      decode j =
        decodeJson j
          # lmap (ExtractBadRequest <<< printJsonDecodeError)
          # pure
          # ExceptT
    in
      jsonBody >>= decode <#> Json # runExceptT

instance RequestParts Buffer where
  extractRequestParts r =
    Request.bodyBuffer r
      <#> lmap (const $ ExtractBadRequest "Expected body")

instance RequestParts Stream where
  extractRequestParts r =
    Request.bodyReadable r
      <#> bimap (const $ ExtractBadRequest "Expected body") Stream
      # liftEffect

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
  extractRequestParts r = runExceptT do
    a <- extractRequestParts @a r # ExceptT
    b <- extractRequestParts @b r # ExceptT
    pure $ a /\ b
