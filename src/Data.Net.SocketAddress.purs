module Data.Net.SocketAddress where

import Data.Either (Either(..))
import Node.Net.Types as Node.Net

data SocketAddress = IPv4 String Int | IPv6 String Int

foreign import nodeAddr ::
  forall (f :: Node.Net.IpFamily). Node.Net.SocketAddress f -> String

foreign import nodePort ::
  forall (f :: Node.Net.IpFamily). Node.Net.SocketAddress f -> Int

fromNode ::
  Either (Node.Net.SocketAddress Node.Net.IPv4)
    (Node.Net.SocketAddress Node.Net.IPv6) ->
  SocketAddress
fromNode (Left a) = IPv4 (nodeAddr a) (nodePort a)
fromNode (Right a) = IPv6 (nodeAddr a) (nodePort a)
