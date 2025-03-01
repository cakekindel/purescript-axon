module Axon where

import Prelude

import Axon.Request (Request)
import Axon.Request.Handler (Handler(..))
import Axon.Request.Handler.Default as Handler.Default
import Axon.Request.Parts.Class (ExtractError(..))
import Axon.Response (Response)
import Axon.Response as Rep
import Axon.Response.Status as Status
import Axon.Runtime (class Runtime)
import Axon.Runtime as Runtime
import Axon.Runtime.Bun as Runtime.Bun
import Control.Monad.Error.Class (throwError)
import Control.Monad.Fork.Class (class MonadFork)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), convertDuration)
import Effect.Aff (Aff, Fiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (Error)
import Effect.Exception as Error
import Prim.Row (class Nub, class Union)
import Record as Record

type Serve m =
  ( port :: Int
  , hostname :: String
  , idleTimeout :: Milliseconds
  , handleError :: Request -> Error -> m Response
  , handleBadRequest :: Request -> String -> m Response
  , handleUnmatched :: Handler m Response
  )

serveDefaults :: forall m. MonadAff m => Record (Serve m)
serveDefaults =
  { port: 8000
  , hostname: "127.0.0.1"
  , idleTimeout: Milliseconds 30000.0
  , handleError: \_ e ->
      let
        rep = Rep.response Status.internalServerError (Rep.BodyString $ show e)
          Map.empty
      in
        liftEffect $ Console.error (show e) $> rep
  , handleBadRequest: \_ e ->
      let
        rep = Rep.response Status.badRequest (Rep.BodyString $ show e) Map.empty
      in
        pure rep
  , handleUnmatched: Handler.Default.notFound
  }

serveToRuntime ::
  forall m.
  MonadUnliftAff m =>
  Handler m Response ->
  Record (Serve m) ->
  Runtime.Init m
serveToRuntime h o =
  let
    fetch :: Boolean -> Handler m Response -> Request -> m Response
    fetch recursed (Handler f) req =
      f req
        >>= case _ of
          Left (ExtractError m) -> o.handleError req (Error.error m)
          Left (ExtractBadRequest m) -> o.handleBadRequest req m
          Left ExtractNext
            | recursed -> liftAff $ throwError $ Error.error
                "Fatal: `serve.handleUnmatched` didn't match request."
            | otherwise -> fetch true o.handleUnmatched req
          Right rep -> pure rep
  in
    { fetch: fetch false h
    , port: Just o.port
    , hostname: Just o.hostname
    , idleTimeout: Just $ convertDuration o.idleTimeout
    }

-- | `serve` using `Bun` in `Aff`
serveBun ::
  forall opts optsMissing optsMerged.
  Union opts optsMissing (Serve Aff) =>
  Union opts (Serve Aff) optsMerged =>
  Nub optsMerged (Serve Aff) =>
  Record opts ->
  Handler Aff Response ->
  Aff (Runtime.Handle Aff Fiber Runtime.Bun.Bun)
serveBun = serve

-- | `serve` in `Aff`
serveAff ::
  forall @rt opts optsMissing optsMerged.
  Runtime rt =>
  Union opts optsMissing (Serve Aff) =>
  Union opts (Serve Aff) optsMerged =>
  Nub optsMerged (Serve Aff) =>
  Record opts ->
  Handler Aff Response ->
  Aff (Runtime.Handle Aff Fiber rt)
serveAff = serve

-- | Runs the server using the given `runtime`.
-- |
-- | First argument (`Record opts`) must be a partial record of `Serve m`.
-- | Omitted fields are set using `serveDefaults`.
-- |
-- | Second argument is your application's `Handler` entrypoint.
serve ::
  forall @runtime f m opts optsMissing optsMerged.
  MonadUnliftAff m =>
  MonadFork f m =>
  Runtime runtime =>
  Union opts optsMissing (Serve m) =>
  Union opts (Serve m) optsMerged =>
  Nub optsMerged (Serve m) =>
  Record opts ->
  Handler m Response ->
  m (Runtime.Handle m f runtime)
serve opts' handle =
  let
    -- Add visible type application to Record.merge
    -- so we can explicitly set intermediate row type
    merge' ::
      forall @r1 @r2 @r3 @r4.
      Union r1 r2 r3 =>
      Nub r3 r4 =>
      Record r1 ->
      Record r2 ->
      Record r4
    merge' = Record.merge

    -- Merge input options with defaults
    opts :: Record (Serve m)
    opts = merge' @_ @_ @optsMerged @_ opts' serveDefaults
  in
    Runtime.serve $ serveToRuntime handle opts
