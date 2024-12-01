module Axon.Request.Parts.Body where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Node.Stream as Stream

newtype Json a = Json a
derive instance Generic (Json a) _
derive instance Newtype (Json a) _
derive newtype instance (Eq a) => Eq (Json a)
derive newtype instance (Ord a) => Ord (Json a)
derive newtype instance (Show a) => Show (Json a)

newtype Stream = Stream (Stream.Readable ())
derive instance Generic Stream _
derive instance Newtype Stream _
