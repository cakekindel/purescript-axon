module Axon.Web.Headers where

import Data.Tuple.Nested (type (/\))
import Effect (Effect)

foreign import data WebHeaders :: Type
foreign import headerEntries :: { tuple :: forall a b. a -> b -> a /\ b } -> WebHeaders -> Effect (Array (String /\ String))
