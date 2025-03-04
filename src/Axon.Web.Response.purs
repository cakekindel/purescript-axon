module Axon.Web.Response where

import Prelude

import Axon.Response (Response(..))
import Axon.Response as Response
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.String.Lower as String.Lower
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

foreign import data WebResponse :: Type

foreign import make ::
  { body :: WebResponseBody, status :: Int, headers :: Object (Array String) } ->
  Effect WebResponse

foreign import data WebResponseBody :: Type

foreign import bodyEmpty :: WebResponseBody

bodyArrayBuffer :: ArrayBuffer -> WebResponseBody
bodyArrayBuffer = unsafeCoerce

bodyReadable :: forall r. Stream.Readable r -> WebResponseBody
bodyReadable = unsafeCoerce

bodyString :: String -> WebResponseBody
bodyString = unsafeCoerce

bodyBuffer :: Buffer -> Effect WebResponseBody
bodyBuffer = map bodyArrayBuffer <<< Buffer.toArrayBuffer

fromResponse :: Response -> Effect WebResponse
fromResponse rep = do
  body' <- case Response.body rep of
    Response.BodyEmpty -> pure bodyEmpty
    Response.BodyBuffer buf -> bodyBuffer buf
    Response.BodyReadable s -> pure $ bodyReadable s
    Response.BodyString s -> pure $ bodyString s
  make
    { body: body'
    , status: unwrap $ Response.status rep
    , headers:
        foldlWithIndex (\k o v -> Object.insert (String.Lower.toString k) v o)
          Object.empty $ Response.headers rep
    }
