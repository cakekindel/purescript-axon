module Axon.Request.Parts.Method where

import Prelude

data Get = Get

derive instance Eq Get
instance Show Get where
  show _ = "Get"

data Post = Post

derive instance Eq Post
instance Show Post where
  show _ = "Post"

data Put = Put

derive instance Eq Put
instance Show Put where
  show _ = "Put"

data Patch = Patch

derive instance Eq Patch
instance Show Patch where
  show _ = "Patch"

data Delete = Delete

derive instance Eq Delete
instance Show Delete where
  show _ = "Delete"

data Options = Options

derive instance Eq Options
instance Show Options where
  show _ = "Options"

data Trace = Trace

derive instance Eq Trace
instance Show Trace where
  show _ = "Trace"

data Connect = Connect

derive instance Eq Connect
instance Show Connect where
  show _ = "Connect"
