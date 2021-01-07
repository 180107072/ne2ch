{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe (isJust)
import Data.Text
import Data.Text.Read
import Data.Typeable (Typeable)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Foundation

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Thread
    name Text
    deriving Typeable
ThreadContent
    title Text
    content Text
    thread_id ThreadId
    deriving Typeable
ThreadReplies
    reply Text
    thread_id ThreadId
    deriving Typeable
|]
