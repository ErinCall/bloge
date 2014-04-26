{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString     (ByteString)
import           Data.Monoid         (mempty)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Snap.Core           (method, Method(..), ifTop)
import           Heist
------------------------------------------------------------------------------
import           Application
import           Splices        (currentPath)
import           Document
import           Document.Heist

index :: Handler App App ()
index = method GET $ render "index"

routes :: [(ByteString, Handler App App ())]
routes = [ ("/",       ifTop index)
         , ("/static", serveDirectory "static")
         ]

app :: [Document] -> SnapletInit App App
app docs = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    let config = mempty {
        hcInterpretedSplices = do
            "currentPath" ## currentPath
            "posts" ## (bindDocuments docs)
      }
    addRoutes routes
    addRoutes $ documentRoutes docs
    addConfig h config
    return $ App h

