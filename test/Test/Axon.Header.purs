module Test.Axon.Header where

import Prelude

import Test.Axon.Header.Typed as Typed
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Header" do
  Typed.spec
