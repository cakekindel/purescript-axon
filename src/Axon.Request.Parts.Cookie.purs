module Axon.Header.Typed.Cookie
  ( Cookies
  , Key
  , Value
  , key
  , value
  , keyToString
  , valueToString
  , parser
  , parse
  , parseE
  , toString
  ) where

import Prelude

import Axon.Header.Typed.Parsing as Parse
import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Data.Either (Either, hush)
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import JSURI (decodeURIComponent, encodeURIComponent)
import Parsing (Parser, runParser)
import Parsing as P
import Parsing.Combinators as P.C
import Parsing.String as P.S
import Parsing.String.Basic as P.S.B

type Cookies = Map Key Value

newtype Key = Key String

derive newtype instance Eq Key
derive newtype instance Show Key
derive newtype instance Ord Key

newtype Value = Value String

derive newtype instance Eq Value
derive newtype instance Show Value
derive newtype instance Ord Value

key :: String -> Maybe Key
key =
  String.trim
    >>> (hush <<< flip runParser Parse.token)
    >>> map Key

value :: String -> Maybe Value
value =
  String.trim
    >>> encodeURIComponent
    >=> (hush <<< map fold <<< flip runParser (P.C.many Parse.cookieChar))
    >>> map Value

keyToString :: Key -> String
keyToString (Key s) = s

valueToString :: Value -> String
valueToString (Value s) = fromMaybe s $ decodeURIComponent s

parser :: Parser String Cookies
parser =
  let
    ws = P.S.B.whiteSpace
    pairs = P.C.sepBy (ws *> pair) (P.S.string ";")
    pair = hush <$> try
      ( pure (/\) <*> (ws *> name) <*>
          (ws *> P.S.string "=" *> ws *> value <* ws)
      )
    name = (P.C.option "" Parse.token) <#> Key
    value = (valueQuoted <|> valueUnquoted) <#> Value
    valueUnquoted = P.C.many Parse.cookieChar <#> fold
    valueQuoted = do
      s <- Parse.quoted
      let len = String.length s
      pure $ String.drop 1 $ String.take (len - 1) s
  in
    (ws *> pairs <* ws) <#> List.catMaybes <#> Map.fromFoldableWith (\_ a -> a)

parseE :: String -> Either P.ParseError Cookies
parseE s = runParser s parser

parse :: String -> Maybe Cookies
parse s = hush $ runParser s parser

toString :: Cookies -> String
toString = fold <<< mapWithIndex cookieToString

cookieToString :: Key -> Value -> String
cookieToString (Key k) (Value v) = k <> "=" <> v <> ";"
