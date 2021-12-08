{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
getConn = ConnectInfo "ec2-18-211-97-89.compute-1.amazonaws.com" -- host
                      5432 -- porta
                      "msogxuqgeppvwt" --user
                      "bfc2871a8c6d5c4fc00fda7c6baba2b9441fe8a2aae1d4f8f1987a356c20a947" -- senha
                      "d4p4og683u49jj" -- banco

migrationCliente :: Query
migrationCliente = "CREATE TABLE IF NOT EXISTS cliente\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, cpf INTEGER NOT NULL, tel INTEGER NOT NULL)"
  
migrationProd :: Query
migrationProd = "CREATE TABLE IF NOT EXISTS produtoz\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, valor REAL NOT NULL, quantidade INTEGER NOT NULL)" 


migrationFornecedor :: Query
migrationFornecedor = "CREATE TABLE IF NOT EXISTS fornecedor\
  \ (id SERIAL PRIMARY KEY, nome TEXT NOT NULL, rg INTEGER NOT NULL, empresa TEXT NOT NULL)" 
  

                       
backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do 
      dbcon <- connect getConn
      serve $ do
          \case 
            ---- gets ---

            BackendRoute_Listar :/ () -> method GET $ do
                res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrationProd
                        query_ dbcon "SELECT * from produtoz"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText res)
            
            BackendRoute_ListarCliente :/ () -> method GET $ do
                res :: [Cliente] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query_ dbcon "SELECT * from cliente"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText res)
            
            BackendRoute_ListarFornecedor :/ () -> method GET $ do
                res :: [Fornecedor] <- liftIO $ do
                        execute_ dbcon migrationFornecedor
                        query_ dbcon "SELECT * from fornecedor"
                modifyResponse $ setResponseStatus 200 "OK"
                writeLazyText (encodeToLazyText res)

            
            
            
             --- fim gets

            --- editar ---
            BackendRoute_Editar :/ pid -> method POST $ do
                prod <- A.decode <$> readRequestBody 2000
                case prod of
                    Just produto -> do
                        liftIO $ do
                            execute_ dbcon migrationProd
                            execute dbcon "UPDATE produtoz SET nome = ?, valor = ?, quantidade = ? WHERE id = ?" 
                                       (produtoNome produto,produtoValor produto,produtoQt produto,pid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            
            ----- EDITAR CLIENTE -----
            BackendRoute_EditarCliente :/ cid -> method POST $ do
                clien <- A.decode <$> readRequestBody 2000
                case clien of
                    Just cliente-> do
                        liftIO $ do
                            execute_ dbcon migrationCliente
                            execute dbcon "UPDATE cliente SET nome = ?, cpf = ?, tel = ? WHERE id = ?" 
                                       (clienteNome cliente, clienteCpf cliente, clienteTel cliente,cid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"

            ---- EDITAR FORNECEDOR ----

            BackendRoute_EditarFornecedor :/ fid -> method POST $ do
                fornecedor<- A.decode <$> readRequestBody 2000
                case fornecedor of
                    Just forn-> do
                        liftIO $ do
                            execute_ dbcon migrationFornecedor
                            execute dbcon "UPDATE fornecedor SET nome = ?, rg = ?, empresa = ? WHERE id = ?" 
                                       (fornecedorNome forn, fornecedorRg forn, fornecedorEmpresa forn,fid)
                        modifyResponse $ setResponseStatus 200 "OK"
                    Nothing -> modifyResponse $ setResponseStatus 500 "ERRO"
            


            --- fim editar ----

            --- deletar ----
            BackendRoute_Apagar :/ pid -> method POST $ do 
                res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrationProd
                        query dbcon "SELECT * from produtoz where id=?" (Only (pid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrationProd
                        execute dbcon "DELETE from produtoz where id=?" (Only (pid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"

            BackendRoute_ApagarCliente :/ cid -> method POST $ do 
                res :: [Cliente] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query dbcon "SELECT * from cliente where id=?" (Only (cid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrationCliente
                        execute dbcon "DELETE from cliente where id=?" (Only (cid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"  
                    
            BackendRoute_ApagarFornecedor :/ fid -> method POST $ do 
                res :: [Fornecedor] <- liftIO $ do
                        execute_ dbcon migrationFornecedor
                        query dbcon "SELECT * from fornecedor where id=?" (Only (fid :: Int))
                if res /= [] then do
                    liftIO $ do
                        execute_ dbcon migrationFornecedor
                        execute dbcon "DELETE from fornecedor where id=?" (Only (fid :: Int))
                    modifyResponse $ setResponseStatus 200 "OK"   
                else
                    modifyResponse $ setResponseStatus 404 "NOT FOUND"  

            --- fim deletar ---

            --- Buscar por id ---
            BackendRoute_Buscar :/ pid -> method GET $ do 
                res :: [Produto] <- liftIO $ do
                        execute_ dbcon migrationProd
                        query dbcon "SELECT * from produtoz where id=?" (Only (pid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND" 

            BackendRoute_BuscarCliente :/ cid -> method GET $ do 
                res :: [Cliente] <- liftIO $ do
                        execute_ dbcon migrationCliente
                        query dbcon "SELECT * from cliente where id=?" (Only (cid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND" 

            
            BackendRoute_BuscarFornecedor:/ fid -> method GET $ do 
                res :: [Fornecedor] <- liftIO $ do
                        execute_ dbcon migrationFornecedor
                        query dbcon "SELECT * from fornecedor where id=?" (Only (fid :: Int))
                if res /= [] then do
                        modifyResponse $ setResponseStatus 200 "OK"   
                        writeLazyText (encodeToLazyText (Prelude.head res))
                else 
                        modifyResponse $ setResponseStatus 404 "NOT FOUND" 

            ---- fim buscar por id ---



            ----- insert ----
            BackendRoute_Produto :/ () -> method POST $ do
                produto <- A.decode <$> readRequestBody 2000
                case produto of
                     Just prod -> do
                         liftIO $ do
                             execute_ dbcon migrationProd
                             execute dbcon 
                                     "INSERT INTO produtoz(nome,valor,quantidade) VALUES (?,?,?)" 
                                     (produtoNome prod, produtoValor prod, produtoQt prod)
                         modifyResponse $ setResponseStatus 200 "OK"    
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
            ---- fim insert produto -----

            ----- insert ----

            BackendRoute_Cliente :/ () -> method POST $ do
                cliente <- A.decode <$> readRequestBody 2000
                case cliente of
                     Just clien -> do
                         liftIO $ do
                             execute_ dbcon migrationCliente
                             execute dbcon 
                                     "INSERT INTO cliente(nome,cpf,tel) VALUES (?,?,?)" 
                                     (clienteNome clien, clienteCpf clien, clienteTel clien )
                         modifyResponse $ setResponseStatus 200 "OK"    
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"

            ---- fim insert cliente -----


              ----- insert ----

            BackendRoute_Fornecedor :/ () -> method POST $ do
                fornecedor <- A.decode <$> readRequestBody 2000
                case fornecedor of
                     Just forn -> do
                         liftIO $ do
                             execute_ dbcon migrationFornecedor
                             execute dbcon 
                                     "INSERT INTO fornecedor(nome,rg,empresa) VALUES (?,?,?)" 
                                     (fornecedorNome forn, fornecedorRg forn, fornecedorEmpresa forn )
                         modifyResponse $ setResponseStatus 200 "OK"    
                     _ -> modifyResponse $ setResponseStatus 500 "Erro"
            
            ---- fim insert fornecedor -----
















             --BackendRoute_Cliente :/ () -> method POST $ do
                --cliente <- A.decode <$> readRequestBody 2000
                --case cliente of
                     --Just clien -> do
                         --liftIO $ do
                             --execute_ dbcon migrationCliente
                            -- execute dbcon 
                                     --"INSERT INTO cliente(nome,cpf,telefone) VALUES (?,?,?)" 
                                     --(clienteNome clien, clienteCpf clien, clienteTelefone clien)
                        -- modifyResponse $ setResponseStatus 200 "OK"    
                    -- _ -> modifyResponse $ setResponseStatus 500 "Erro"
            _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }