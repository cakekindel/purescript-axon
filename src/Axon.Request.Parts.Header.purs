module Axon.Request.Parts.Header where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.String.Lower (StringLower)

newtype Header a = Header a

derive instance Generic (Header a) _
derive instance Newtype (Header a) _
derive newtype instance (Eq a) => Eq (Header a)
derive newtype instance (Ord a) => Ord (Header a)
derive newtype instance (Show a) => Show (Header a)

newtype HeaderMap = HeaderMap (Map StringLower (Array String))

derive instance Generic HeaderMap _
derive instance Newtype HeaderMap _
derive newtype instance Eq HeaderMap
derive newtype instance Ord HeaderMap
derive newtype instance Show HeaderMap
