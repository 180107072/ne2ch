{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Data.Text
import Database.Persist.Sqlite
import Yesod
import Yesod.Static

staticFiles "./static"

data App = App
  { getStatic :: Static,
    sqlBackend :: SqlBackend
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB f = do
    App s conn <- getYesod
    runSqlConn f conn
