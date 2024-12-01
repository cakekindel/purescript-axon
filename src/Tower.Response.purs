module Tower.Response (Response, response, body, status, headers, withHeader, withBody, withStatus, fromStatus, ok, module Body) where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.String.Lower (StringLower)
import Data.String.Lower as String.Lower
import Tower.Response.Body (Body(..))
import Tower.Response.Body (Body(..), formBody) as Body

data Response = Response {body :: Body, headers :: Map StringLower String, status :: Int}

response :: Int -> Body -> Map String String -> Response
response s b h = Response {status: s, body: b, headers: h # foldlWithIndex (\k m v -> Map.insert (String.Lower.fromString k) v m) Map.empty}

status :: Response -> Int
status (Response a) = a.status

body :: Response -> Body
body (Response a) = a.body

headers :: Response -> Map StringLower String
headers (Response a) = a.headers

withHeader :: String -> String -> Response -> Response
withHeader k v (Response a) = Response $ a {headers = Map.insert (String.Lower.fromString k) v a.headers}

withStatus :: Int -> Response -> Response
withStatus s (Response a) = Response $ a {status = s}

withBody :: Body -> Response -> Response
withBody b (Response a) = Response $ a {body = b}

fromStatus :: Int -> Response
fromStatus s = Response {body: BodyEmpty, headers: Map.empty, status: s}

ok :: Response
ok = fromStatus 200
