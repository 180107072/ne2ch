{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.ThreadR where

import Control.Monad.IO.Class (liftIO)
import Data.Text
import Database
import Database.Persist.Sqlite
import Database.Persist.TH
import Foundation
import Yesod

getThreadR :: Text -> Handler Html
getThreadR name = defaultLayout [whamlet|<h1>Hello #{name}!|]

--Maybe Text to Text convertion
postThreadR name = do
  name <- lookupPostParam "name"
  title <- lookupPostParam "title"
  content <- lookupPostParam "content"
  case name of
    Just _name -> do
      case title of
        Just _title -> do
          case content of
            Just _content -> do
              thread <- runDB $ insert $ Thread _name
              runDB $ insert $ ThreadContent _title _content thread
              return $ object ["msg" .= "Thread Created"]
            Nothing -> return $ object ["msg" .= "Content is empty"]
        Nothing -> return $ object ["msg" .= "Title is empty"]
    Nothing -> return $ object ["msg" .= "Name is empty"]
