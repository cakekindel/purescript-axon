module Test.Axon.Request where

import Prelude

import Test.Axon.Request.Parts as Parts
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Request" do
  Parts.spec
