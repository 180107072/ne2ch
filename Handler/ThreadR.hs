{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Handler.ThreadR where

import Text.Read
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Database
import Database.Persist.Sqlite
import Database.Persist.TH
import Foundation
import Yesod
import Database.Persist ()
import Data.Maybe
import qualified Data.String


type NameT = Maybe Text
type TitleT = Maybe Text
type ContentT = Maybe Text
data ThreadType = ThreadType NameT TitleT ContentT


gEntity :: [Entity Thread] -> Either (Entity Thread) Text
gEntity (x:xs) = Left x
gEntity [] = Right "Thread is not set yet"

preparePost :: Data.String.IsString p => Maybe p -> p
preparePost (Just t) = t
preparePost Nothing = error "Some of Fields is Empty"


getThreadR :: Text -> Handler Html
getThreadR name = do
  thread <- runDB $ selectList [ThreadName ==. name] []
  let entityThread = gEntity thread
  case entityThread of
    Left _entity -> do
      let nameT = threadName $ entityVal _entity
      liftIO $ print (nameT :: Text)
      defaultLayout [whamlet|<h1>Thread #{nameT}!|]
    Right msg -> do
      defaultLayout [whamlet|<h1>#{msg}|]

postThreadR :: (PersistStoreWrite (YesodPersistBackend site), YesodPersist site, BaseBackend (YesodPersistBackend site) ~ SqlBackend) => p -> HandlerFor site Value
postThreadR _ = do
  name <-  lookupPostParam "name"
  title <- lookupPostParam "title"
  content <- lookupPostParam "content"
  let n = preparePost name
  let t = preparePost title
  let c = preparePost content
  thread <- runDB $ insert $ Thread n
  runDB $ insert $ ThreadContent t c thread
  return $ object ["msg" .= "Thread Created"]
  -- case name of
  --   Just _name -> do
  --     case title of
  --       Just _title -> do
  --         case content of
  --           Just _content -> do
  --             thread <- runDB $ insert $ Thread _name
  --             runDB $ insert $ ThreadContent _title _content thread
  --             return $ object ["msg" .= "Thread Created"]
  --           Nothing -> return $ object ["msg" .= "Content is empty"]
  --       Nothing -> return $ object ["msg" .= "Title is empty"]
  --   Nothing -> return $ object ["msg" .= "Name is empty"]
