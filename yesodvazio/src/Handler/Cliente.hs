{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Cliente where

import Import

formCliente :: Maybe Cliente -> Form Cliente
formCliente mc = renderDivs $ Cliente
    <$> areq textField "Nome: " (fmap clienteNome mc)
    <*> areq textField "Cpf: " (fmap clienteCpf mc)
    <*> areq intField "Idade: " (fmap clienteIdade mc)

getClienteR :: Handler Html
getClienteR = do
    (widget,_) <- generateFormPost (formCliente Nothing)
    msg <- getMessage
    defaultLayout (formWidget widget msg ClienteR "Cadastrar")

postClienteR :: Handler Html
postClienteR = do
    ((result,_),_) <- runFormPost (formCliente Nothing)
    case result of
        FormSuccess cliente -> do
            runDB $ insert cliente
            setMessage [shamlet|
                <div>
                    Cliente incluido
            |]
            redirect ClienteR
        _ -> redirect HomeR

getPerfilR :: ClienteId -> Handler Html
getPerfilR cid = do
    cliente <- runDB $ get404 cid
    defaultLayout [whamlet|
        <h1>
            Pagina de #{clienteNome cliente}
        <h2>
            CPF: #{clienteCpf cliente}
        <h2>
            Idade: #{clienteIdade cliente}
    |]

getListaCliR :: Handler Html
getListaCliR = do
    clientes <- runDB $ selectList [] [Asc ClienteNome]
    defaultLayout $ do
        $(whamletFile "templates/clientes.hamlet")

postApagarCliR :: ClienteId -> Handler Html
postApagarCliR cid = do
    runDB $ delete cid
    redirect ListaCliR

getEditarCliR :: ClienteId -> Handler Html
getEditarCliR cid = do
    cliente <- runDB $ get404 cid
    (widget,_) <- generateFormPost (formCliente (Just cliente))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarCliR cid) "Editar")

postEditarCliR :: ClienteId -> Handler Html
postEditarCliR cid = do
    clienteAntigo <- runDB $ get404 cid
    ((result,_),_) <- runFormPost (formCliente Nothing)
    case result of
        FormSuccess novoCliente -> do
            runDB $ replace cid novoCliente
            redirect ListaCliR
        _ -> redirect HomeR

formWidget :: Widget -> Maybe Html -> Route App -> Text -> Widget
formWidget widget msg rota m = $(whamletFile "templates/form.hamlet")