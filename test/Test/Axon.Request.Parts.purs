module Test.Axon.Request.Parts where

import Prelude

import Axon.Header.Typed (ContentType)
import Axon.Request (Request)
import Axon.Request as Request
import Axon.Request.Handler (invokeHandler)
import Axon.Request.Method (Method(..))
import Axon.Request.Parts.Class
  ( ExtractError(..)
  , Header
  , Json(..)
  , Patch
  , Path(..)
  , Post(..)
  , Try(..)
  , extractRequestParts
  )
import Axon.Request.Parts.Path (type (/), IgnoreRest)
import Control.Monad.Error.Class (liftEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Net.SocketAddress as SocketAddress
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL as URL
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Net.SocketAddress as SocketAddress
import Node.Stream as Stream
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.MIME as MIME

spec :: Spec Unit
spec = describe "Parts" do
  it "extracts the whole request" do
    req <- liftEffect $ Request.make
      { body: Request.BodyEmpty
      , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
      , headers: Map.singleton "content-type" "application/json"
      , address: SocketAddress.IPv4 "127.0.0.1" 81
      , method: GET
      }
    _ :: Request <- invokeHandler (pure @Aff) req <#> lmap (error <<< show) >>=
      liftEither
    pure unit

  it "extracts header, method, path, JSON body" do
    stream <- Buffer.fromString """{"firstName": "henry"}""" UTF8
      >>= Stream.readableFromBuffer
      # liftEffect
    req <- liftEffect $ Request.make
      { body: Request.BodyReadable stream
      , url: URL.fromString "http://localhost:80/users/12" # unsafePartial
          fromJust
      , headers: Map.singleton "content-type" "application/json"
      , address: SocketAddress.IPv4 "127.0.0.1" 81
      , method: PATCH
      }

    let
      handler ::
        Patch ->
        Header (ContentType MIME.Json) ->
        Path ("users" / Int) Int ->
        Json { firstName :: String } ->
        Aff String
      handler _ _ (Path id) (Json { firstName }) = do
        id `shouldEqual` 12
        firstName `shouldEqual` "henry"
        pure firstName

    name <- invokeHandler handler req
      <#> lmap (error <<< show)
      >>= liftEither

    name `shouldEqual` "henry"

  describe "Path" do
    it "matches a route matching literal" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Path "foo" _) req <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Path unit)

    it "matches a route matching multiple literals" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/bar/baz" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / "bar" / "baz") _) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Path unit)

    it "does not partially match a route ..." do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/bar/baz" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / "bar") _) req
      a `shouldEqual` (Left ExtractNext)

    it "... but does if ends in IgnoreRest" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/bar/baz" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / "bar" / IgnoreRest) _) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Path unit)

    it "extracts an int" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/123/bar" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / Int / "bar") _) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Path 123)

    it "extracts an int and a string" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/123/bar/baz" #
            unsafePartial fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / Int / "bar" / String) _) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Path $ 123 /\ "baz")

  describe "Body" do
    it "extracts a string body from a cached string" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Try Request.BodyStringError String) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Ok "foo")

    it "extracts a string body from a readable stream" do
      stream <- Buffer.fromString "foo" UTF8 >>= Stream.readableFromBuffer #
        liftEffect
      req <- liftEffect $ Request.make
        { body: Request.BodyReadable stream
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Try Request.BodyStringError String) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Ok "foo")

      a' <- extractRequestParts @String req <#> lmap (error <<< show)
        >>= liftEither
      a' `shouldEqual` "foo"

    it "extracts a string body from a buffer" do
      buf <- Buffer.fromString "foo" UTF8 # liftEffect
      req <- liftEffect $ Request.make
        { body: Request.BodyCached buf
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: GET
        }
      a <- extractRequestParts @(Try Request.BodyStringError String) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Ok "foo")

      a' <- extractRequestParts @String req <#> lmap (error <<< show)
        >>= liftEither
      a' `shouldEqual` "foo"

    it "extracts a JSON body" do
      stream <- Buffer.fromString """{"foo": 123, "bar": "abc"}""" UTF8
        >>= Stream.readableFromBuffer
        # liftEffect
      req <- liftEffect $ Request.make
        { body: Request.BodyReadable stream
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: SocketAddress.IPv4 "127.0.0.1" 81
        , method: POST
        }
      a <- extractRequestParts @(Post /\ Json { foo :: Int, bar :: String }) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` (Post /\ Json { foo: 123, bar: "abc" })
