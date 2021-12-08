{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

data Produto = Produto {
    produtoId :: Int,
    produtoNome :: Text,
    produtoValor :: Double,
    produtoQt :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Cliente = Cliente {
    clienteId :: Int,
    clienteNome :: Text,
    clienteCpf :: Int,
    clienteTel :: Int
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)

data Fornecedor = Fornecedor {
    fornecedorId :: Int,
    fornecedorNome :: Text,
    fornecedorRg :: Int,
    fornecedorEmpresa :: Text
} deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)


