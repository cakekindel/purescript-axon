module Axon.Response.Body where

import Prelude

import Node.Buffer (Buffer)
import Node.Stream as Stream
import Prim.Row (class Cons)
import Unsafe.Coerce (unsafeCoerce)

data Body
  = BodyEmpty
  | BodyString String
  | BodyBuffer Buffer
  | BodyReadable (Stream.Readable ())

instance Show Body where
  show BodyEmpty = "BodyEmpty"
  show (BodyString s) = "BodyString " <> show s
  show (BodyBuffer _) = "BodyBuffer _"
  show (BodyReadable _) = "BodyReadable _"

string :: String -> Body
string = BodyString

buffer :: Buffer -> Body
buffer = BodyBuffer

stream :: forall a a'. Cons "read" Stream.Read a' a => Stream.Stream a -> Body
stream =
  let
    narrow :: Stream.Stream a -> Stream.Readable ()
    narrow = unsafeCoerce
  in
    BodyReadable <<< narrow

empty :: Body
empty = BodyEmpty
