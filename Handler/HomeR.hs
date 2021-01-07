{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.HomeR where

import Conduit
import Control.Monad.IO.Class (liftIO)
import Data.Text
import qualified Data.Text.IO as FS
import Database
import Database.Persist.Sqlite
import Database.Persist.TH
import Foundation
import Yesod

htpath :: FilePath
htpath = "./view/index.html"

getHomeR :: Handler Html
getHomeR = do
  htcontent <- liftIO (FS.readFile htpath)
  return $ preEscapedToMarkup htcontent
