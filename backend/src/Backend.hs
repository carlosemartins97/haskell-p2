{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import Common.Route
import Obelisk.Backend
import Database.PostgreSQL.Simple
import Data.Text
import Obelisk.Route
import Snap.Core
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Common.Api
import Data.Aeson.Text





getConn :: ConnectInfo
getConn  = ConnectInfo "ec2-23-21-4-7.compute-1.amazonaws.com"
                        5432-- porta
                        "fvuzzrjppfnvcb"
                        "ad2ebd8af8b6545c71eea377ddbf72344d92ad0095b03820e5a2c1b8f90637bb"
                        "degi38qm6vqluh"


migration :: Query
migration = "CREATE TABLE IF NOT EXISTS exame (cd_exame SERIAL PRIMARY KEY, nm_exame TEXT NOT NULL, vl_exame DOUBLE PRECISION, qt_exame INTEGER NOT NULL)"




backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    dbcon <- connect getConn

    serve $ do
      \case
          BackendRoute_Exame :/ () -> method POST $ do
            exame <- A.decode <$> readRequestBody 2000
            case exame of
                Just exa -> do
                  liftIO $ do
                    execute_ dbcon migration
                    execute dbcon
                      "INSERT INTO exame(nm_exame,vl_exame,qt_exame) VALUES (?,?,?)"
                      (nm_exame exa, vl_exame exa, qt_exame exa)
                  modifyResponse $ setResponseStatus 200 "OK"
                _ -> modifyResponse $ setResponseStatus 500 "Erro"
          BackendRoute_Listar :/ () -> method GET $ do
            res :: [Exame] <- liftIO $ do
                execute_ dbcon migration
                query_ dbcon "SELECT * from exame"
            modifyResponse $ setResponseStatus 200 "OK"
            writeLazyText (encodeToLazyText res)
          _ -> return ()
          
  , _backend_routeEncoder = fullRouteEncoder
  }






