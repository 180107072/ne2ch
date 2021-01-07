{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Maybe (isJust)
import Data.Text
import Data.Text.Read
import Data.Typeable (Typeable)
import Database
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Foundation
import Handler.HomeR
import Handler.ThreadR
import System.Environment
import Yesod
import Yesod.Static

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = runNoLoggingT $
  withSqliteConn "ne2ch.db3" $ \conn -> liftIO $ do
    runSqlConn (runMigration migrateAll) conn
    s <- static "./static"
    warp 3000 $ App s conn
