module Test.Axon.Request.Handler where

import Axon.Request.Parts.Class
import Prelude

import Axon.Request (Body)
import Axon.Request as Request
import Axon.Request.Handler as Handle
import Axon.Request.Handler.Default (notFound)
import Axon.Request.Method (Method(..))
import Axon.Request.Parts.Path (type (/))
import Axon.Response (Response)
import Axon.Response as Response
import Axon.Response.Body as Response.Body
import Axon.Response.Construct (toResponse)
import Axon.Response.Construct as Response.Construct
import Axon.Response.Status as Status
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.URL (URL)
import Data.URL as URL
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Net.SocketAddress as SocketAddress
import Node.Net.Types (IPv4, IPv6, SocketAddress)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

defaultRequest ::
  { headers :: Map String String
  , address :: Either (SocketAddress IPv4) (SocketAddress IPv6)
  , url :: URL
  , method :: Method
  , body :: Body
  }
defaultRequest =
  { body: Request.BodyEmpty
  , url: URL.fromString "http://localhost:80/" # unsafePartial fromJust
  , headers: Map.singleton "content-type" "application/json"
  , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
      { address: "127.0.0.1", port: 81 }
  , method: GET
  }

getPerson :: Get -> Path ("people" / Int) Int -> Aff Response
getPerson _ (Path id) =
  if id == 1 then
    liftEffect $ toResponse $ Response.Construct.Json { name: "Henry" }
  else
    liftEffect $ toResponse $ Status.notFound

spec :: Spec Unit
spec = describe "Handler" do
  it "responds ok" do
    req <- liftEffect $ Request.make $ defaultRequest
      { url = defaultRequest.url `URL.(/)` "people" `URL.(/)` "1" }
    rep <- Handle.invokeHandler (getPerson `Handle.or` notFound) req
      <#> lmap (error <<< show)
      >>= liftEither
    Response.status rep `shouldEqual` Status.ok
  it "responds not found" do
    req <- liftEffect $ Request.make $ defaultRequest
      { url = defaultRequest.url `URL.(/)` "people" `URL.(/)` "1"
      , method = PUT
      }
    rep <- Handle.invokeHandler (getPerson `Handle.or` notFound) req
      <#> lmap (error <<< show)
      >>= liftEither
    Response.status rep `shouldEqual` Status.notFound
