module Axon.Serve.Bun where

import Axon.Request.Handler (Handler)
import Axon.Response (Response)
import Axon.Runtime as Runtime
import Axon.Runtime.Bun as Runtime.Bun
import Axon.Serve (Serve)
import Axon.Serve as Serve
import Effect.Aff (Aff)
import Prim.Row (class Nub, class Union)

serve ::
  forall opts optsMissing optsMerged.
  Union opts optsMissing (Serve Aff) =>
  Union opts (Serve Aff) optsMerged =>
  Nub optsMerged (Serve Aff) =>
  Record opts ->
  Handler Aff Response ->
  Aff (Runtime.Handle Aff Runtime.Bun.Bun)
serve = Serve.serve
