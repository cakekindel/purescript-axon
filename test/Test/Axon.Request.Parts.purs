module Test.Axon.Request.Parts where

import Prelude

import Axon.Request (Request)
import Axon.Request as Request
import Axon.Request.Method (Method(..))
import Axon.Request.Parts.Class
  ( Json(..)
  , Patch(..)
  , Path(..)
  , Post(..)
  , extractRequestParts
  )
import Axon.Request.Parts.Path (type (/), IgnoreRest)
import Control.Monad.Error.Class (liftEither, liftMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL as URL
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

spec :: Spec Unit
spec = describe "Parts" do
  it "extracts the whole request" do
    req <- liftEffect $ Request.make
      { body: Request.BodyEmpty
      , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
      , headers: Map.empty
      , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
          { address: "127.0.0.1", port: 81 }
      , method: GET
      }
    void $ extractRequestParts @Request req <#> lmap (error <<< show)
      >>= liftEither
      >>= liftMaybe (error "was nothing")

  it "extracts method, path, JSON body" do
    stream <- Buffer.fromString """{"firstName": "henry"}""" UTF8
      >>= Stream.readableFromBuffer
      # liftEffect
    req <- liftEffect $ Request.make
      { body: Request.BodyReadable stream
      , url: URL.fromString "http://localhost:80/users/12" # unsafePartial
          fromJust
      , headers: Map.empty
      , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
          { address: "127.0.0.1", port: 81 }
      , method: PATCH
      }
    a <-
      extractRequestParts
        @(Patch ((Path ("users" / Int) _) /\ Json { firstName :: String }))
        req <#> lmap (error <<< show) >>= liftEither >>= liftMaybe
        (error "was nothing")
    a `shouldEqual` Patch (Path 12 /\ Json { firstName: "henry" })

  describe "Path" do
    it "matches a route matching literal" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Path "foo" _) req <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Path unit)

    it "matches a route matching multiple literals" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/bar/baz" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / "bar" / "baz") _) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Path unit)

    it "does not partially match a route ..." do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/bar/baz" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / "bar") _) req
        <#> lmap (error <<< show)
        >>= liftEither
      a `shouldEqual` Nothing

    it "... but does if ends in IgnoreRest" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/bar/baz" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / "bar" / IgnoreRest) _) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Path unit)

    it "extracts an int" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/123/bar" # unsafePartial
            fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / Int / "bar") _) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Path 123)

    it "extracts an int and a string" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo/123/bar/baz" #
            unsafePartial fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Path ("foo" / Int / "bar" / String) _) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Path $ 123 /\ "baz")

  describe "Body" do
    it "extracts a string body from a cached string" do
      req <- liftEffect $ Request.make
        { body: Request.BodyCachedString "foo"
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Either Request.BodyStringError String) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Right "foo")

    it "extracts a string body from a readable stream" do
      stream <- Buffer.fromString "foo" UTF8 >>= Stream.readableFromBuffer #
        liftEffect
      req <- liftEffect $ Request.make
        { body: Request.BodyReadable stream
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Either Request.BodyStringError String) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Right "foo")

      a' <- extractRequestParts @String req <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a' `shouldEqual` "foo"

    it "extracts a string body from a buffer" do
      buf <- Buffer.fromString "foo" UTF8 # liftEffect
      req <- liftEffect $ Request.make
        { body: Request.BodyCached buf
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: GET
        }
      a <- extractRequestParts @(Either Request.BodyStringError String) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` (Right "foo")

      a' <- extractRequestParts @String req <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a' `shouldEqual` "foo"

    it "extracts a JSON body" do
      stream <- Buffer.fromString """{"foo": 123, "bar": "abc"}""" UTF8
        >>= Stream.readableFromBuffer
        # liftEffect
      req <- liftEffect $ Request.make
        { body: Request.BodyReadable stream
        , url: URL.fromString "http://localhost:80/foo" # unsafePartial fromJust
        , headers: Map.empty
        , address: Left $ unsafePerformEffect $ SocketAddress.newIpv4
            { address: "127.0.0.1", port: 81 }
        , method: POST
        }
      a <- extractRequestParts @(Post (Json { foo :: Int, bar :: String })) req
        <#> lmap (error <<< show)
        >>= liftEither
        >>= liftMaybe (error "was nothing")
      a `shouldEqual` Post (Json { foo: 123, bar: "abc" })
