{-# LANGUAGE OverloadedStrings #-}

module Document.Heist
  ( documentRoutes
  , bindDocuments
  , tagRoutes
  ) where

import           Prelude                     hiding (lookup)
import qualified Heist.Interpreted           as I
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Heist
import           Data.HashMap                (keys, empty, lookup)
import qualified Data.ByteString             as B
import           Data.List                   (intersperse)
import           Data.Maybe                  (fromJust)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
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
  "tagListing"     ## I.runNodeList $ if (null $ dTags d) then [] else
      [ X.TextNode "Posted in " ]
      ++ (intersperse (X.TextNode ", ") $ map tagLink $ dTags d) ++
      [ X.TextNode "." ]
    where
      tagLink tag = X.Element "a"
                              [("href", "/t/" `T.append` tag)]
                              [X.TextNode tag]



tagRoutes :: [Document] -> [(B.ByteString, Handler App App ())]
tagRoutes docs = [(pathOf tag, renderTag tag) | tag <- keys docMap]
  where
    pathOf tag = "/t/" `B.append` T.encodeUtf8 tag
    docMap = foldr insertDoc empty docs
    renderTag tag = renderWithSplices "tagResults" $ do
        "posts" ## bindDocuments $ fromJust $ lookup tag docMap
        "tagName" ## I.textSplice tag
