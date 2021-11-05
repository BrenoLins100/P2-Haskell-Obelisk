{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route

menu :: DomBuilder t m => m ()
menu = do
    el "menu" $ do
        el "ul" $ do
            el "li" $ do
                el "a" (text "Link")
            el "li" $ do
                el "a" (text "Link")
            el "li" $ do
                el "a" (text "Link")
    elAttr "div" ("class" =: "container") $ do
        el "h1" (text "Bem vindo")

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Olá mundo"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      -- Chamando menu
      menu

      elAttr "div" ("class" =: "imagem-dog") $ do
        elAttr "img" ("src" =: static @"doge.jpg" <> "class" =: "cachorro") blank

      el "div" $ do
          exampleConfig <- getConfig "common/example"
          case exampleConfig of
              Nothing -> text "O Arquivo não foi encontrado /:"
              Just s -> text $ T.decodeUtf8 s

      return ()
  }
