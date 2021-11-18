{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Database.PostgreSQL.Simple 
import Snap.Core
import qualified Data.Aeson as A
import Data.Text
import Control.Monad.IO.Class (liftIO)

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS cliente\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL)"

getConn :: ConnectInfo
getConn = ConnectInfo "ec2-18-211-97-89.compute-1.amazonaws.com" -- host
                      5432 -- porta
                      "msogxuqgeppvwt" --user
                      "bfc2871a8c6d5c4fc00fda7c6baba2b9441fe8a2aae1d4f8f1987a356c20a947" -- senha
                      "d4p4og683u49jj" -- banco

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
        dbcon <- connect getConn
        serve $ \case 
            BackendRoute_Cliente :/ () -> do
                Just nome <- A.decode <$> readRequestBody 2000
                liftIO $ do 
                     execute_ dbcon migration
                     execute dbcon "INSERT INTO cliente (nome) VALUES (?)" [nome :: Text]
                modifyResponse $ setResponseStatus 200 "OK"
            _ -> return ()
        return ()
  , _backend_routeEncoder = fullRouteEncoder
  }
