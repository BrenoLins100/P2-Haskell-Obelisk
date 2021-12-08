{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

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
import Data.Aeson (ToJSON)
import Common.Api
import Common.Route
import Control.Monad.Fix

-------------- EXEMPLO BACKEND -----------------------

getPath :: R BackendRoute -> T.Text
getPath p = renderBackendRoute checFullREnc p

-----------------------------------------------------

sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados


-----CADASTRAR PRODUTO------

reqProd :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqProd = do
    el "p" (text "Cadastrar Produto")
    elAttr "div" ("class" =: "container insere") $ do
    

    el "p" (text "Nome:") 
    nome <- inputElement def


    el "p" (text "Valor:") 
    vl <- numberInput

   
    el "p" (text "Quantidade:") 
    qt <- numberInput

    let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
    (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-warning btn-insere-prod") (text "Inserir")
    let click = domEvent Click submitBtn
    let prodEvt = tag (current prod) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Produto :/ ()) <$> prodEvt))
    return ()
     
------FIM CADASTRAR PRODUTO------


-----CADASTRAR Cliente------

reqCliente :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqCliente = do
    el "p" (text "Cadastrar Cliente")
    elAttr "div" ("class" =: "container insere") $ do


    el "p" (text "Nome:") 
    nome <- inputElement def

  
    el "p" (text "CPF:") 
    cpf <- numberInput

    el "p" (text "Telefone:") 
    tel <- numberInput

    let clien = fmap (\((n,v),q) -> Cliente 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) cpf) tel)
    (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-warning") (text "Inserir")
    let click = domEvent Click submitBtn
    let clienEvt = tag (current clien) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Cliente :/ ()) <$> clienEvt))
    return ()
     
------ FIM CADASTRAR CLIENTE-----


-----CADASTRAR Fornecedor-----

reqFornecedor :: ( DomBuilder t m
       , Prerender js t m
       ) => m ()
reqFornecedor = do

    el "p" (text "Cadastrar Fornecedor")
    
    elAttr "div" ("class" =: "container insere") $ do
 
    el "p" (text "Nome:") 
    nome <- inputElement def
    el "p" (text "RG:") 
    rg <- numberInput 
    el "p" (text "Empresa:") 
    empresa <- inputElement def

    let forn = fmap (\((n,v),q) -> Fornecedor 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) rg) (_inputElement_value empresa))
    (submitBtn,_) <- elAttr' "button" ("class" =: "btn btn-warning") (text "Inserir")
    let click = domEvent Click submitBtn
    let fornEvt = tag (current forn) click
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (sendRequest (BackendRoute_Fornecedor :/ ()) <$> fornEvt))
    return ()
     
------ FIM CADASTRAR CLIENTE-----


-----------LISTANDO----------
getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def    

getListReq2 :: XhrRequest ()
getListReq2 = xhrRequest "GET" (getPath (BackendRoute_ListarCliente :/ ())) def   

getListReq3 :: XhrRequest ()
getListReq3 = xhrRequest "GET" (getPath (BackendRoute_ListarFornecedor :/ ())) def   


------------------- CRIANDO TABELA LISTANDO PRODUTOS ------------------------

data Acao = Perfil Int | Editar Int | Excluir Int

tabRegistro :: (PostBuild t m, DomBuilder t m) => Dynamic t Produto -> m (Event t Acao)
tabRegistro pr = do 
    el "tr" $ do

        ---- TD DA TABELA ----
        el "td" (dynText $ fmap (T.pack . show . produtoId) pr)
        el "td" (dynText $ fmap (T.pack . show .produtoNome) pr)
        el "td" (dynText $ fmap (T.pack . show . produtoValor) pr)
        el "td" (dynText $ fmap (T.pack . show . produtoQt) pr)  

        ----- BOTAO QUE LEVA AO PERFIL consultar -----
        elAttr "span" ("class" =: "btn-consultar" <> "onclick" =: "mostrar2()") $do
        evt <- fmap (fmap (const Perfil)) (button "Consultar")

        ---- BOTAO QUE LEVA AO EDITAR -----
        evt2 <- fmap (fmap (const Editar)) (button "editar")

        evt3 <- fmap (fmap (const Excluir)) (button "Excluir")

        return (attachPromptlyDynWith (flip ($)) (fmap produtoId pr) (leftmost [evt,evt2,evt3]))
        
reqTabela :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabela = Workflow $ do
    elAttr "div" ("class" =: "container listar") $ do
    btn <- button "Listar Produtos"
    prods :: Dynamic t (Event t (Maybe [Produto])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> btn))
    evt <- return (fmap (fromMaybe []) $ switchDyn prods)
    dynP <- foldDyn (++) [] evt
    tb <- elAttr "table" ("class" =: "table align-middle table-striped") $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Valor")
                el "th" (text "Quantidade")
                el "th" blank
                el "th" blank
                el "th" blank
        
        el "tbody" $ do
             simpleList dynP tabRegistro
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
        escolherPag (Perfil pid) = pagPerfil pid
        escolherPag (Editar pid) = editarPerfil pid
        escolherPag (Excluir pid) = excluirPerfil pid

getProdReq :: Int -> XhrRequest ()
--- BUSCANDO PRODUTO POR ID -------
getProdReq pid = xhrRequest "GET" (getPath (BackendRoute_Buscar :/ pid)) def 


pagPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil pid = Workflow $ do
    ----------AO CLICAR EM CONSULTAR ----------------
    elAttr "div" ("class" =: "container perfil") $ do
    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getProdReq pid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn prod)
    dynP <- return ((fromMaybe (Produto 0 "" 0 0)) <$> mdyn)
    el "div" $ do
        elAttr "div" ("class" =: "nome-produto") (dynText $ fmap produtoNome dynP)
        elAttr "div" ("class" =: "valor-produto") (dynText $ fmap (T.pack . show . produtoValor) dynP)
        elAttr "div" ("class" =: "qt-produto") (dynText $ fmap (T.pack . show . produtoQt) dynP)
    elAttr "div" ("class" =: "btn-voltar" <> "onclick" =: "mostrar()") $ do
    ret <- button "voltar"
    return ("Id: " <> (T.pack $ show pid), reqTabela <$ ret)        

numberInputDyn :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

editarPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil pid = Workflow $ do

    ----- BOTAO MOSTRAR DADOS PARA EDITAR------
    elAttr "div" ("class" =: "container perfil insere") $ do
    btn <- button "mostrar"
    prod :: Dynamic t (Event t (Maybe Produto)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getProdReq pid) <$> btn))
    mdyn <- return (switchDyn prod)
    dynE <- return ((fromMaybe (Produto 0 "" 0 0)) <$> mdyn)
    
    elAttr "div" ("class" =: "container perfil insere nome-produto") $ do
    nome <- inputElement $ 
         def & inputElementConfig_setValue .~ (fmap produtoNome dynE)
    elAttr "div" ("class" =: "container perfil insere valor-produto") $ do
    vl <- numberInputDyn (fmap produtoValor dynE)
    elAttr "div" ("class" =: "container perfil insere qt-produto") $ do
    qt <- numberInputDyn (fmap produtoQt dynE)
    
    let prod = fmap (\((n,v),q) -> Produto 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) vl) qt)
    ---- BOTAO MOSTRAR DADOS PARA EDITAR------

    ------BOTAO CONFIRMA EDITAR------    
    elAttr "div" ("class" =: "container perfil2") $ do
    elAttr "div" ("class" =: "btn-confirmar" <> "onclick" =: "mostrar()") $ do
    submitBtn <- button "Confirmar Edição"
    let prodEvt = tag (current prod) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_Editar :/ pid) 
            <$> prodEvt)) 
    return ("Id: " <> (T.pack $ show pid), reqTabela <$ submitBtn)
      
    ------FIM BOTAO CONFIRMA EDITAR------  

excluirPerfil :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
excluirPerfil  pid = Workflow $ do
    elAttr "div" ("class" =: "container perfil2") $ do      
    elAttr "h1" ("class" =: "title") (text "Deseja confirmar a exclusão do produto?")    
    (btnSim,x) <- elAttr' "button" ("class"=: "btn btn-danger" <> "onclick" =: "mostrar()") (text "Confirmar")
    let simEvt = domEvent Click btnSim
    x :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_Apagar :/ pid) 
            <$> simEvt))    

    return ("Id: " <> (T.pack $ show pid), reqTabela <$  simEvt)
                 
reqLista :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista = do
    r <- workflow reqTabela
    el "div" (dynText r)
------------------------------FIM TABELA PRODUTO-------------------------------------------


------------------- CRIANDO TABELA LISTANDO CLIENTES ------------------------

data Acao2 = Perfil2 Int | Editar2 Int | Excluir2 Int

tabRegistro2 :: (PostBuild t m, DomBuilder t m) => Dynamic t Cliente -> m (Event t Acao2)
tabRegistro2 cr = do 
    el "tr" $ do

        ---- TD DA TABELA ----
        el "td" (dynText $ fmap (T.pack . show . clienteId) cr)
        el "td" (dynText $ fmap (T.pack . show .clienteNome) cr)
        el "td" (dynText $ fmap (T.pack . show . clienteCpf) cr)
        el "td" (dynText $ fmap (T.pack . show . clienteTel) cr)  

        ----- BOTAO QUE LEVA AO PERFIL consultar -----
        elAttr "span" ("class" =: "btn-consultar" <> "onclick" =: "mostrar2()") $do
        evt2 <- fmap (fmap (const Perfil2)) (button "Consultar")

        ---- BOTAO QUE LEVA AO EDITAR -----
        evt22 <- fmap (fmap (const Editar2)) (button "editar")

        ---- BOTAO QUE LEVA DELETA -----
        evt222 <- fmap (fmap (const Excluir2)) (button "Deletar")

        return (attachPromptlyDynWith (flip ($)) (fmap clienteId cr) (leftmost [evt2,evt22,evt222]))
        
reqTabela2 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabela2 = Workflow $ do
    elAttr "div" ("class" =: "container listar") $ do
    btn <- button "Listar Clientes"
    cliens :: Dynamic t (Event t (Maybe [Cliente])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq2 <$> btn))
    evt2 <- return (fmap (fromMaybe []) $ switchDyn cliens)
    dynP <- foldDyn (++) [] evt2
    tb <- elAttr "table" ("class" =: "table align-middle table-striped") $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "Cpf")
                el "th" (text "Telefone")
                el "th" blank
                el "th" blank
                el "th" blank
        
        el "tbody" $ do
             simpleList dynP tabRegistro2
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
        escolherPag (Perfil2 cid) = pagPerfil2 cid
        escolherPag (Editar2 cid) = editarPerfil2 cid
        escolherPag (Excluir2 cid) = excluirPerfil2 cid

getProdReq2 :: Int -> XhrRequest ()
--- BUSCANDO CLIENTE POR ID -------
getProdReq2 cid = xhrRequest "GET" (getPath (BackendRoute_BuscarCliente :/ cid)) def 


pagPerfil2 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil2 cid = Workflow $ do
    ----------AO CLICAR EM CONSULTAR ----------------
    elAttr "div" ("class" =: "container perfil") $ do
    btn <- button "mostrar"
    clien :: Dynamic t (Event t (Maybe Cliente)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getProdReq2 cid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn clien)
    dynP <- return ((fromMaybe (Cliente 0 "" 0 0)) <$> mdyn)
    el "div" $ do
        elAttr "div" ("class" =: "nome-cliente") (dynText $ fmap clienteNome dynP)
        elAttr "div" ("class" =: "cpf-cliente") (dynText $ fmap (T.pack . show . clienteCpf) dynP)
        elAttr "div" ("class" =: "tel-cliente") (dynText $ fmap (T.pack . show . clienteTel) dynP)

    elAttr "div" ("class" =: "btn-voltar" <> "onclick" =: "mostrar()") $ do
    ret2 <- button "voltar"
    return ("Id: " <> (T.pack $ show cid), reqTabela2 <$ ret2)        

numberInputDyn2 :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn2 p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

editarPerfil2 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil2 cid = Workflow $ do

    ----- BOTAO MOSTRAR DADOS PARA EDITAR------
    elAttr "div" ("class" =: "container perfil insere") $ do
    btn <- button "mostrar"
    clien :: Dynamic t (Event t (Maybe Cliente)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getProdReq2 cid) <$> btn))
    mdyn <- return (switchDyn clien)
    dynE <- return ((fromMaybe (Cliente 0 "" 0 0)) <$> mdyn)
    
    elAttr "div" ("class" =: "container perfil insere nome-cliente") $ do
    nome <- inputElement $ 
         def & inputElementConfig_setValue .~ (fmap clienteNome dynE)
    elAttr "div" ("class" =: "container perfil insere cpf-cliente") $ do
    cpf <- numberInputDyn (fmap clienteCpf dynE)
    elAttr "div" ("class" =: "container perfil insere tel-cliente") $ do
    tel <- numberInputDyn (fmap clienteTel dynE)
    
    let clien = fmap (\((n,v),q) -> Cliente 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) cpf) tel)
    ---- BOTAO MOSTRAR DADOS PARA EDITAR------

    ------BOTAO CONFIRMA EDITAR------    
    elAttr "div" ("class" =: "container perfil2") $ do
    elAttr "div" ("class" =: "btn-confirmar" <> "onclick" =: "mostrar()") $ do
    submitBtn <- button "Confirmar Edição"
    let clienEvt = tag (current clien) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_EditarCliente :/ cid) 
            <$> clienEvt)) 
    return ("Id: " <> (T.pack $ show cid), reqTabela2 <$ submitBtn)

--- BOTAO DELETA -----
excluirPerfil2 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
excluirPerfil2  cid = Workflow $ do
    elAttr "div" ("class" =: "container perfil2") $ do      
    elAttr "h1" ("class" =: "title") (text "Deseja confirmar a exclusão do cliente ?")    
    (btnSim,x) <- elAttr' "button" ("class"=: "btn btn-danger" <> "onclick" =: "mostrar()") (text "Confirmar")
    let simEvt = domEvent Click btnSim
    x :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_ApagarCliente :/ cid) 
            <$> simEvt))    

    return ("Id: " <> (T.pack $ show cid), reqTabela2 <$  simEvt)
      
    ------BOTAO CONFIRMA EDITAR------  
                 
reqLista2 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista2 = do
    r <- workflow reqTabela2
    el "div" (dynText r)



------------------------------FIM TABELA CLIENTE-------------------------------------------


------------------- CRIANDO TABELA LISTANDO FORNECEDORES------------------------

data Acao3 = Perfil3 Int | Editar3 Int | Excluir3 Int

tabRegistro3 :: (PostBuild t m, DomBuilder t m) => Dynamic t Fornecedor -> m (Event t Acao3)
tabRegistro3 fr = do 
    el "tr" $ do

        ---- TD DA TABELA ----
        el "td" (dynText $ fmap (T.pack . show . fornecedorId) fr)
        el "td" (dynText $ fmap (T.pack . show .fornecedorNome) fr)
        el "td" (dynText $ fmap (T.pack . show . fornecedorRg) fr)
        el "td" (dynText $ fmap (T.pack . show . fornecedorEmpresa) fr)  

        ----- BOTAO QUE LEVA AO PERFIL consultar -----
        elAttr "span" ("class" =: "btn-consultar" <> "onclick" =: "mostrar2()") $do
        evt3 <- fmap (fmap (const Perfil3)) (button "Consultar")

        ---- BOTAO QUE LEVA AO EDITAR -----
        evt33 <- fmap (fmap (const Editar3)) (button "editar")

        ---- BOTAO QUE LEVA DELETA -----
        evt333 <- fmap (fmap (const Excluir3)) (button "Deletar")

        return (attachPromptlyDynWith (flip ($)) (fmap fornecedorId fr) (leftmost [evt3,evt33,evt333]))
        
reqTabela3 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Workflow t m T.Text
reqTabela3= Workflow $ do
    elAttr "div" ("class" =: "container listar") $ do
    btn <- button "Listar Fornecedores"
    forns :: Dynamic t (Event t (Maybe [Fornecedor])) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const getListReq3 <$> btn))
    evt3 <- return (fmap (fromMaybe []) $ switchDyn forns)
    dynP <- foldDyn (++) [] evt3
    tb <- elAttr "table" ("class" =: "table align-middle table-striped") $ do
        el "thead" $ do
            el "tr" $ do
                el "th" (text "Id")
                el "th" (text "Nome")
                el "th" (text "RG")
                el "th" (text "Empresa")
                el "th" blank
                el "th" blank
                el "th" blank
        
        el "tbody" $ do
             simpleList dynP tabRegistro3
    tb' <- return $ switchDyn $ fmap leftmost tb
    return ("Listagem", escolherPag <$> tb')
    where
        escolherPag (Perfil3 fid) = pagPerfil3 fid
        escolherPag (Editar3 fid) = editarPerfil3 fid
        escolherPag (Excluir3 fid) = excluirPerfil3 fid

getProdReq3 :: Int -> XhrRequest ()
--- BUSCANDO FORNECEDOR POR ID -------
getProdReq3 fid = xhrRequest "GET" (getPath (BackendRoute_BuscarFornecedor :/ fid)) def 


pagPerfil3 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
pagPerfil3 fid = Workflow $ do
    ----------AO CLICAR EM CONSULTAR ----------------
    elAttr "div" ("class" =: "container perfil") $ do
    btn <- button "mostrar"
    forn :: Dynamic t (Event t (Maybe Fornecedor)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync (const (getProdReq3 fid) <$> btn))
    mdyn <- holdDyn Nothing (switchDyn forn)
    dynP <- return ((fromMaybe (Fornecedor 0 "" 0 "")) <$> mdyn)
    el "div" $ do
        elAttr "div" ("class" =: "nome-fornecedor") (dynText $ fmap fornecedorNome dynP)
        elAttr "div" ("class" =: "rg-fornecedor") (dynText $ fmap (T.pack . show . fornecedorRg) dynP)
        elAttr "div" ("class" =: "empresa-fornecedor") (dynText $ fmap (T.pack . show . fornecedorEmpresa) dynP)
    elAttr "div" ("class" =: "btn-voltar" <> "onclick" =: "mostrar()") $ do
    ret3 <- button "voltar"
    return ("Id: " <> (T.pack $ show fid), reqTabela3 <$ ret3)        

numberInputDyn3 :: (DomBuilder t m, Num a, Read a, Show a) =>
               Event t a -> m (Dynamic t a)
numberInputDyn3 p = do
      val <- return (fmap (T.pack . show) p)
      n <- inputElement $ def
        & inputElementConfig_setValue .~ val
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)

editarPerfil3 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
editarPerfil3 fid = Workflow $ do

    ----- BOTAO MOSTRAR DADOS PARA EDITAR------
    elAttr "div" ("class" =: "container perfil insere") $ do
    btn <- button "mostrar"
    forn :: Dynamic t (Event t (Maybe Fornecedor)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> performRequestAsync
           (const (getProdReq3 fid) <$> btn))
    mdyn <- return (switchDyn forn)
    dynE <- return ((fromMaybe (Fornecedor 0 "" 0 "")) <$> mdyn)
    
    elAttr "div" ("class" =: "container perfil insere nome-fornecedor") $ do
    nome <- inputElement $ 
         def & inputElementConfig_setValue .~ (fmap fornecedorNome dynE)
    elAttr "div" ("class" =: "container perfil insere rg-fornecedor") $ do
    rg <- numberInputDyn (fmap fornecedorRg dynE)
    elAttr "div" ("class" =: "container perfil insere empresa-fornecedor") $ do
    empresa <- inputElement $ 
         def & inputElementConfig_setValue .~ (fmap fornecedorEmpresa dynE)
    
    let forn = fmap (\((n,v),q) -> Fornecedor 0 n v q) (zipDyn (zipDyn (_inputElement_value nome) rg) (_inputElement_value empresa))
    ---- BOTAO MOSTRAR DADOS PARA EDITAR------

    ------BOTAO CONFIRMA EDITAR------    
    elAttr "div" ("class" =: "container perfil2") $ do
    elAttr "div" ("class" =: "btn-confirmar" <> "onclick" =: "mostrar()") $ do
    submitBtn <- button "Confirmar Edição"
    let fornEvt = tag (current forn) submitBtn
    _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_EditarFornecedor :/ fid) 
            <$> fornEvt)) 
    return ("Id: " <> (T.pack $ show fid), reqTabela3 <$ submitBtn)

--- BOTAO DELETA -----
excluirPerfil3 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => Int -> Workflow t m T.Text
excluirPerfil3  fid = Workflow $ do
    elAttr "div" ("class" =: "container perfil2") $ do      
    elAttr "h1" ("class" =: "title") (text "Deseja confirmar a exclusão do fornecedor ?")    
    (btnSim,x) <- elAttr' "button" ("class"=: "btn btn-danger" <> "onclick" =: "mostrar()") (text "Confirmar")
    let simEvt = domEvent Click btnSim
    x :: Dynamic t (Event t (Maybe T.Text)) <- prerender
        (pure never)
        (fmap decodeXhrResponse <$> 
            performRequestAsync (sendRequest (BackendRoute_ApagarFornecedor :/ fid) 
            <$> simEvt))    

    return ("Perfil: " <> (T.pack $ show fid), reqTabela3 <$  simEvt)
      
    ------BOTAO CONFIRMA EDITAR------  
                 
reqLista3 :: ( DomBuilder t m
            , Prerender js t m
            , MonadHold t m
            , MonadFix m
            , PostBuild t m) => m ()
reqLista3 = do
    r <- workflow reqTabela3
    el "div" (dynText r)



------------------------------FIM TABELA CLIENTE-------------------------------------------






data Pagina = Pagina0 | Pagina1 | Pagina2 | Pagina3 | Pagina4 | Pagina5 | Pagina6 | Pagina7

numberInput :: (Read a, Num a) => DomBuilder t m => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) $ _inputElement_value n

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
    (ev, _) <- el' "li" (elAttr "a" ("href" =: "#" <> "onclick" =: "mostrar()") (text t))
    return ((\_ -> p) <$> domEvent Click ev)
    
menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
    evs <- el "ul" $ do
        p1 <- clickLi Pagina1 "Integrantes "
        p2 <- clickLi Pagina2 "Cadastrar Clientes"
        p3 <- clickLi Pagina3 "Listar Clientes" 
        p4 <- clickLi Pagina4 "Cadastrar Produtos"
        p5 <- clickLi Pagina5 "Listar Produtos"
        p6 <- clickLi Pagina6 "Cadastrar Fornecedor"
        p7 <- clickLi Pagina7 "Listar Fornecedor"
        return (leftmost [p1,p2,p3,p4,p5,p6,p7])
    holdDyn Pagina0 evs    
    
currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m, MonadFix m) => Pagina -> m ()
currPag p = 
    case p of
         Pagina0 -> blank
         Pagina1 -> elAttr "div" ("class" =: "inte") $ do
             el "p" (text "Breno Lins")
             el "p" (text "Marcelo de Castro")
         Pagina2 -> reqCliente
         Pagina3 -> reqLista2
         Pagina4 -> reqProd
         Pagina5 -> reqLista
         Pagina6 -> reqFornecedor
         Pagina7 -> reqLista3
         
mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js0 t m, MonadFix m) => m ()
mainPag = do
    pag <- elAttr "menu" ("class" =: "menu") menuLi
    el "h1" (text "Bem vindo")
    dyn_ $ currPag <$> pag

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Projeto P2 - Controle de Vendas"
      -- bootstrap.min.css --
      -- Meta tag viewport --
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank
      elAttr "meta" ("charset" =: "charset=\"UTF-8\"") blank
      elAttr "link" ("href" =: static @"main.css"  <>  "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "link" ("href" =: static @"bootstrap.min.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("type"=:"text/javascript") (text "function mostrar(){setTimeout(()=>{document.querySelector('.container.listar button').click()},450)};function mostrar2(){setTimeout(()=>{document.querySelector('.container.perfil button').click()},450)}")
  , _frontend_body = do
      mainPag
  }
