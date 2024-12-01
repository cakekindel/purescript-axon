module Tower.Request.Parts.Path where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL (URL)
import Type.Prelude (Proxy(..))

newtype Path :: Type -> Type -> Type
newtype Path a b = Path b

data Sep :: Type -> Type -> Type
data Sep a b

data IgnoreRest :: Type
data IgnoreRest

infixl 9 type Sep as /
infixl 9 type IgnoreRest as ...

class PathParts :: forall a. a -> Type -> Constraint
class PathParts a b | a -> b where
  extractPathParts :: URL -> Array String -> Maybe (Array String /\ b)

instance (PathParts aa ab, PathParts ba bb) => PathParts (aa / ba) (ab /\ bb) where
  extractPathParts u segments = do
    segments' /\ ab <- extractPathParts @aa u segments
    segments'' /\ bb <- extractPathParts @ba u segments'
    pure $ segments'' /\ ab /\ bb
else instance PathParts (...) Unit where
  extractPathParts _ _ = Just $ [] /\ unit
else instance PathParts String String where
  extractPathParts _ segments = do
    head <- Array.head segments
    pure $ (fromMaybe [] (Array.tail segments) /\ head)
else instance PathParts Int Int where
  extractPathParts _ segments = do
    head <- Array.head segments
    a <- Int.fromString head
    pure $ (fromMaybe [] (Array.tail segments) /\ a)
else instance (IsSymbol k) => PathParts k Unit where
  extractPathParts _ segments = do
    head <- Array.head segments
    guard $ head == reflectSymbol (Proxy @k)
    pure $ (fromMaybe [] (Array.tail segments) /\ unit)
