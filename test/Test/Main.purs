module Test.Main where

import Prelude

import Effect (Effect)
import Test.Axon.Request as Test.Request
import Test.Axon.Header as Test.Header
import Test.Spec (describe)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ specReporter ] $ describe "Axon" do
  Test.Request.spec
  Test.Header.spec
