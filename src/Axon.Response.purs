module Axon.Response
  ( Response(..)
  , response
  , body
  , status
  , headers
  , withHeader
  , withBody
  , withStatus
  , fromStatus
  , module Body
  ) where

import Prelude

import Axon.Response.Body (Body(..))
import Axon.Response.Body (Body(..)) as Body
import Axon.Response.Status (Status)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Show.Generic (genericShow)
import Data.String.Lower (StringLower)
import Data.String.Lower as String.Lower

data Response = Response
  { body :: Body, headers :: Map StringLower String, status :: Status }

derive instance Generic Response _
instance Semigroup Response where
  append a b = Response
    { body: body b
    , status: status b
    , headers: Map.union (headers a) (headers b)
    }

instance Show Response where
  show = genericShow

response :: Status -> Body -> Map String String -> Response
response s b h = Response
  { status: s
  , body: b
  , headers: h # foldlWithIndex
      (\k m v -> Map.insert (String.Lower.fromString k) v m)
      Map.empty
  }

status :: Response -> Status
status (Response a) = a.status

body :: Response -> Body
body (Response a) = a.body

headers :: Response -> Map StringLower String
headers (Response a) = a.headers

withHeader :: String -> String -> Response -> Response
withHeader k v (Response a) = Response $ a
  { headers = Map.insert (String.Lower.fromString k) v a.headers }

withStatus :: Status -> Response -> Response
withStatus s (Response a) = Response $ a { status = s }

withBody :: Body -> Response -> Response
withBody b (Response a) = Response $ a { body = b }

fromStatus :: Status -> Response
fromStatus s = Response { body: BodyEmpty, headers: Map.empty, status: s }
