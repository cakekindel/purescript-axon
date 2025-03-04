module Test.Main where

import Prelude

import Axon.Request.Handler as Handler
import Axon.Request.Parts.Class (Delete, Get, Path(..), Post)
import Axon.Request.Parts.Path (type (/))
import Axon.Response (Response)
import Axon.Response.Construct (Json(..), toResponse)
import Axon.Response.Status as Status
import Data.Filterable (filter)
import Data.Foldable (elem)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Axon.Header as Test.Header
import Test.Axon.Request as Test.Request
import Test.Spec (describe, pending')
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ specReporter ] $ describe "Axon" do
  Test.Request.spec
  Test.Header.spec
