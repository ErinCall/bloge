{-# LANGUAGE OverloadedStrings #-}

module Document.Heist where

import qualified Heist.Interpreted  as I
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import           Data.Either.Utils  (fromRight)
import           Data.Text.Encoding as T
import qualified Data.ByteString    as B
import qualified Text.XmlHtml       as X
import           Application
import           Document

documentRoutes :: [Document] -> [(B.ByteString, Handler App App ())]
documentRoutes docs = [(pathOf doc, renderDoc doc) | doc <- docs]
    where
        pathOf doc = "/p/" `B.append` (T.encodeUtf8 $ dSlug doc)
        renderDoc doc = renderWithSplices "post" (documentSplices doc)

documentSplices :: Monad n => Document -> Splices (I.Splice n)
documentSplices d = do
  "postTitle" ## I.textSplice (dTitle d)
  "postBody"  ## I.runNodeList $ parseBody $ dBody d
    where
      parseBody = X.docContent . fromRight . X.parseHTML "" . T.encodeUtf8
