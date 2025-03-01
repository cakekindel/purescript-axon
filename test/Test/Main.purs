module Test.Main where

import Prelude

import Axon as Axon
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

  pending' "example" do
    cheeses :: Ref.Ref (Array String) <- liftEffect $ Ref.new
      [ "cheddar", "swiss", "gouda" ]

    let
      getCheeses :: Get -> Path ("cheeses") _ -> Aff Response
      getCheeses _ _ = liftEffect do
        cheeses' <- Ref.read cheeses
        toResponse $ Status.ok /\ Json cheeses'

      deleteCheese :: Delete -> Path ("cheeses" / String) _ -> Aff Response
      deleteCheese _ (Path id) = liftEffect do
        cheeses' <- Ref.read cheeses
        if (not $ elem id cheeses') then
          toResponse $ Status.notFound
        else do
          Ref.modify_ (filter (_ /= id)) cheeses
          toResponse $ Status.accepted

      postCheese :: Post -> Path "cheeses" _ -> String -> Aff Response
      postCheese _ _ cheese =
        let
          tryInsert as
            | elem cheese as = { state: as, value: false }
            | otherwise = { state: as <> [ cheese ], value: true }
        in
          liftEffect
            $ Ref.modify' tryInsert cheeses
            >>=
              if _ then
                toResponse $ Status.accepted
              else
                toResponse $ Status.conflict

    handle <-
      Axon.serveBun
        { port: 8080, hostname: "localhost" }
        (getCheeses `Handler.or` postCheese `Handler.or` deleteCheese)

    Aff.joinFiber $ handle.join
