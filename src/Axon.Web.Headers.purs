module Axon.Web.Headers where

import Prelude

import Data.Bifunctor (rmap)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)

foreign import data WebHeaders :: Type
foreign import headerEntries ::
  { tuple :: forall a b. a -> b -> a /\ b } ->
  WebHeaders ->
  Effect (Array (String /\ String))

toMap :: WebHeaders -> Effect (Map String (Array String))
toMap hs =
  headerEntries { tuple: (/\) } hs
    <#> map (rmap pure)
    <#> Map.fromFoldableWith append
