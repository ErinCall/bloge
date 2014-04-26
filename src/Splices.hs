
module Splices where

import           Control.Monad.Trans.Class    (lift)
import           Heist.Interpreted            as I
import           Snap.Core
import           Data.Text.Encoding           (decodeUtf8)
import           Data.ByteString              (append)
import qualified Text.XmlHtml as X

currentPath :: MonadSnap m => I.Splice m
currentPath = do
  requestPath <- lift $ withRequest $ \request -> do
    let url  = rqURI request
        host = rqServerName request
    return (host `append` url)

  return [X.TextNode $ decodeUtf8 $ urlEncode requestPath]
