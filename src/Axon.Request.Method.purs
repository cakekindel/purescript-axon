module Axon.Request.Method where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String

data Method = GET | POST | PUT | PATCH | DELETE | OPTIONS | TRACE | CONNECT

derive instance Generic Method _
derive instance Eq Method
instance Show Method where
  show = genericShow

methodToString :: Method -> String
methodToString GET = "GET"
methodToString POST = "POST"
methodToString PUT = "PUT"
methodToString PATCH = "PATCH"
methodToString DELETE = "DELETE"
methodToString OPTIONS = "OPTIONS"
methodToString TRACE = "TRACE"
methodToString CONNECT = "CONNECT"

methodFromString :: String -> Maybe Method
methodFromString =
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
    go <<< String.toUpper
