{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Data.Maybe
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Text.Read (readMaybe)
import Reflex.Dom.Core

import Common.Api
import Common.Route

-------------- EXEMPLO BACKEND -----------------------

getPath :: T.Text
getPath = renderBackendRoute checFullREnc $ BackendRoute_Cliente :/ ()

nomeRequest :: T.Text -> XhrRequest T.Text
nomeRequest s = postJson getPath (Cliente s)

pagReq :: ( DomBuilder t m
          , Prerender js t m
          ) => m (Event t T.Text)
pagReq = do
    inpnome <- inputElement def
    (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-warning") (text "Inserir")
    let click = domEvent Click submitBtn
    let nm = tag (current $ _inputElement_value inpnome) click
    st <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (nomeRequest <$> nm))
    return (fromMaybe "" <$> switchDyn st) 
    
paginaInsere :: ( DomBuilder t m
       , PostBuild t m
       , MonadHold t m
       , Prerender js t m
       ) => m ()
paginaInsere = do
    el "p" (text "Inserir registro")
    elAttr "div" ("class" =: "container insere") $ do
        st <- pagReq 
        tx <- holdDyn "" st
        el "div" (dynText tx)
------------------------------------------------------

data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        p1 <- clickLi Pagina1 "Exemplo 1:"
        p2 <- clickLi Pagina2 "Exemplo 2:"
        p3 <- clickLi Pagina3 "Exemplo 3:"
        p4 <- clickLi Pagina4 "Inserir no BD"
        return (leftmost [p1,p2,p3,p4])
    holdDyn Pagina0 evs    
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => Pagina -> m ()
currPag p = 
    case p of
         Pagina0 -> blank
         Pagina1 -> elAttr "img" ("src" =: static @"doge.jpg" <> "class" =: "cachorro") blank
         Pagina2 -> el "div" (text "Oi mundo")
         Pagina3 -> el "div" (text "Oi mundo")
         Pagina4 -> paginaInsere
         
mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m) => m ()
mainPag = do
    pag <- elAttr "menu" ("class" =: "menu") menuLi
    el "h1" (text "Bem vindo")
    dyn_ $ currPag <$> pag
-- bootstrap.min.css
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Olá Mundo"
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank
      elAttr "link" ("href" =: static @"main.css"  <>  "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      mainPag
  }
