{-# LANGUAGE OverloadedStrings #-}

module Document.Heist
  ( documentRoutes
  , bindDocuments
  ) where

import qualified Heist.Interpreted           as I
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import qualified Data.ByteString             as B
import           Data.Text                   as T
import           Data.Text.Encoding          as T
import           Data.Time.ISO8601           (formatISO8601)
import           Text.Blaze.Renderer.XmlHtml (renderHtml)
import qualified Text.XmlHtml                as X
import           Application
import           Document

documentRoutes :: [Document] -> [(B.ByteString, Handler App App ())]
documentRoutes docs = [(pathOf doc, renderDoc doc) | doc <- docs]
    where
        pathOf doc = "/p/" `B.append` (T.encodeUtf8 $ dSlug doc)
        renderDoc doc = renderWithSplices "post" (documentSplices doc)

bindDocuments :: Monad n => [Document] -> I.Splice n
bindDocuments = I.mapSplices $ I.runChildrenWith . documentSplices

documentSplices :: Monad n => Document -> Splices (I.Splice n)
documentSplices d = do
  "postTitle"      ## I.textSplice (dTitle d)
  "postSlug"       ## I.textSplice (dSlug d)
  "postPostedDate" ## I.textSplice (T.pack $ formatISO8601 $ dPosted d)
  "postBody"       ## I.runNodeList $ X.docContent $ renderHtml $ dBody d
