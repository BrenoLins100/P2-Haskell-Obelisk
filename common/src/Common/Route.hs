{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text, unpack)
import Data.Function
import Data.Functor.Identity
import Common.Api

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Cliente  :: BackendRoute () 
  BackendRoute_Produto :: BackendRoute () 
  BackendRoute_Fornecedor :: BackendRoute () 
  BackendRoute_Listar :: BackendRoute () -- Listar produto
  BackendRoute_ListarCliente :: BackendRoute ()
  BackendRoute_ListarFornecedor :: BackendRoute ()
  BackendRoute_Buscar :: BackendRoute Int -- Buscar produto
  BackendRoute_BuscarCliente :: BackendRoute Int
  BackendRoute_BuscarFornecedor :: BackendRoute Int
  BackendRoute_Apagar :: BackendRoute Int -- apagar produto 
  BackendRoute_ApagarCliente :: BackendRoute Int
  BackendRoute_ApagarFornecedor :: BackendRoute Int
  BackendRoute_Editar :: BackendRoute Int -- editar produto
  BackendRoute_EditarCliente :: BackendRoute Int
  BackendRoute_EditarFornecedor :: BackendRoute Int

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

checFullREnc
  :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & \case
  Left err -> error $ unpack err
  Right encoder -> encoder    
  
fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Cliente -> PathSegment "cliente" $ unitEncoder mempty
      BackendRoute_Produto -> PathSegment "produto" $ unitEncoder mempty
      BackendRoute_Fornecedor -> PathSegment "fornecedor" $ unitEncoder mempty
      BackendRoute_Listar -> PathSegment "listar" $ unitEncoder mempty
      BackendRoute_ListarCliente -> PathSegment "listarcliente" $ unitEncoder mempty
      BackendRoute_ListarFornecedor -> PathSegment "listarfornecedor" $ unitEncoder mempty
      BackendRoute_Buscar -> PathSegment "buscar" readShowEncoder
      BackendRoute_BuscarCliente -> PathSegment "buscarcliente" readShowEncoder
      BackendRoute_BuscarFornecedor -> PathSegment "buscarfornecedor" readShowEncoder
      BackendRoute_Apagar -> PathSegment "apagar" readShowEncoder
      BackendRoute_ApagarCliente -> PathSegment "apagarcliente" readShowEncoder
      BackendRoute_ApagarFornecedor -> PathSegment "apagarfornecedor" readShowEncoder
      BackendRoute_EditarCliente -> PathSegment "editarcliente" readShowEncoder
      BackendRoute_EditarFornecedor -> PathSegment "editarfornecedor" readShowEncoder
      BackendRoute_Editar -> PathSegment "editar" readShowEncoder)
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
