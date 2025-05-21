module Axon.Serve.Node where

import Axon.Request.Handler (Handler)
import Axon.Response (Response)
import Axon.Runtime as Runtime
import Axon.Runtime.Node as Runtime.Node
import Axon.Serve (Serve, serve)
import Effect.Aff (Aff)
import Prim.Row (class Nub, class Union)

serveHTTP1 ::
  forall opts optsMissing optsMerged.
  Union opts optsMissing (Serve Aff) =>
  Union opts (Serve Aff) optsMerged =>
  Nub optsMerged (Serve Aff) =>
  Record opts ->
  Handler Aff Response ->
  Aff (Runtime.Handle Aff Runtime.Node.Server)
serveHTTP1 = serve
