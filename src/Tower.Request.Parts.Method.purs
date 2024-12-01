module Tower.Request.Parts.Method where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)

newtype Get a = Get a
derive instance Generic (Get a) _
derive instance Newtype (Get a) _
derive newtype instance (Eq a) => Eq (Get a)
derive newtype instance (Ord a) => Ord (Get a)
derive newtype instance (Show a) => Show (Get a)

newtype Post a = Post a
derive instance Generic (Post a) _
derive instance Newtype (Post a) _
derive newtype instance (Eq a) => Eq (Post a)
derive newtype instance (Ord a) => Ord (Post a)
derive newtype instance (Show a) => Show (Post a)

newtype Put a = Put a
derive instance Generic (Put a) _
derive instance Newtype (Put a) _
derive newtype instance (Eq a) => Eq (Put a)
derive newtype instance (Ord a) => Ord (Put a)
derive newtype instance (Show a) => Show (Put a)

newtype Patch a = Patch a
derive instance Generic (Patch a) _
derive instance Newtype (Patch a) _
derive newtype instance (Eq a) => Eq (Patch a)
derive newtype instance (Ord a) => Ord (Patch a)
derive newtype instance (Show a) => Show (Patch a)

newtype Delete a = Delete a
derive instance Generic (Delete a) _
derive instance Newtype (Delete a) _
derive newtype instance (Eq a) => Eq (Delete a)
derive newtype instance (Ord a) => Ord (Delete a)
derive newtype instance (Show a) => Show (Delete a)

newtype Options a = Options a
derive instance Generic (Options a) _
derive instance Newtype (Options a) _
derive newtype instance (Eq a) => Eq (Options a)
derive newtype instance (Ord a) => Ord (Options a)
derive newtype instance (Show a) => Show (Options a)

newtype Trace a = Trace a
derive instance Generic (Trace a) _
derive instance Newtype (Trace a) _
derive newtype instance (Eq a) => Eq (Trace a)
derive newtype instance (Ord a) => Ord (Trace a)
derive newtype instance (Show a) => Show (Trace a)

newtype Connect a = Connect a
derive instance Generic (Connect a) _
derive instance Newtype (Connect a) _
derive newtype instance (Eq a) => Eq (Connect a)
derive newtype instance (Ord a) => Ord (Connect a)
derive newtype instance (Show a) => Show (Connect a)
