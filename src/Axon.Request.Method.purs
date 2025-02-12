module Axon.Request.Method where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String

data Method
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | OPTIONS
  | TRACE
  | CONNECT
  | HEAD

derive instance Generic Method _
derive instance Eq Method
instance Show Method where
  show = genericShow

toString :: Method -> String
toString GET = "GET"
toString HEAD = "HEAD"
toString POST = "POST"
toString PUT = "PUT"
toString PATCH = "PATCH"
toString DELETE = "DELETE"
toString OPTIONS = "OPTIONS"
toString TRACE = "TRACE"
toString CONNECT = "CONNECT"

fromString :: String -> Maybe Method
fromString =
  let
    go "GET" = Just GET
    go "POST" = Just POST
    go "PUT" = Just PUT
    go "PATCH" = Just PATCH
    go "DELETE" = Just DELETE
    go "OPTIONS" = Just OPTIONS
    go "TRACE" = Just TRACE
    go "CONNECT" = Just CONNECT
    go _ = Nothing
  in
    go
