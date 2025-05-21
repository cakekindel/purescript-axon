module Axon.Request.Parts.Path where

import Prelude

import Control.Alternative (guard)
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.URL (URL)
import Type.Prelude (Proxy(..))

data Path :: forall k. k -> Type -> Type
data Path a b = Path b

derive instance Generic (Path a b) _
derive instance Eq b => Eq (Path a b)
instance Show b => Show (Path a b) where
  show = genericShow

data Sep :: forall ka kb. ka -> kb -> Type
data Sep a b

data IgnoreRest :: Type
data IgnoreRest

infixr 9 type Sep as /
infixl 9 type IgnoreRest as ...

class DiscardTupledUnits :: Type -> Type -> Constraint
class DiscardTupledUnits a b | a -> b where
  discardTupledUnits :: a -> b

instance (DiscardTupledUnits a b) => DiscardTupledUnits (Unit /\ a) b where
  discardTupledUnits (_ /\ a) = discardTupledUnits a
else instance (DiscardTupledUnits a b) => DiscardTupledUnits (a /\ Unit) b where
  discardTupledUnits (a /\ _) = discardTupledUnits a
else instance
  ( DiscardTupledUnits aa ab
  , DiscardTupledUnits ba bb
  ) =>
  DiscardTupledUnits (aa /\ ba) (ab /\ bb) where
  discardTupledUnits (a /\ b) = discardTupledUnits a /\ discardTupledUnits b
else instance DiscardTupledUnits a a where
  discardTupledUnits = identity

class PathParts :: forall k. k -> Type -> Constraint
class PathParts a b | a -> b where
  extractPathParts :: URL -> Array String -> Maybe (Array String /\ b)

instance (PathParts aa ab, PathParts ba bb) => PathParts (aa / ba) (ab /\ bb) where
  extractPathParts u segments = do
    segments' /\ ab <- extractPathParts @aa u segments
    segments'' /\ bb <- extractPathParts @ba u segments'
    pure $ segments'' /\ ab /\ bb
else instance PathParts IgnoreRest Unit where
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
