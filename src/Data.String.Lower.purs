module Data.String.Lower where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.String as String

newtype StringLower = StringLower String

derive instance Generic StringLower _
derive newtype instance Show StringLower
derive newtype instance Eq StringLower
derive newtype instance Ord StringLower
derive newtype instance Monoid StringLower
derive newtype instance Semigroup StringLower

fromString :: String -> StringLower
fromString = StringLower <<< String.toLower

toString :: StringLower -> String
toString (StringLower a) = a
