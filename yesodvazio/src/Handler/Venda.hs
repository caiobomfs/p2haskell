{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Venda where

import Import
import Database.Persist.Postgresql

formVenda :: ClienteId -> Form Venda
formVenda cid = renderDivs $ Venda
    <$> pure cid
    <*> areq (selectField prodCB) "Produto: " Nothing
    <*> lift (liftIO (map utctDay getCurrentTime))
    <*> areq intField "Quantidade: " Nothing

prodCB :: Handler (OptionList (Key Produto))
prodCB = do
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    optionsPairs $
        map (\r -> (produtoNome $ entityVal r, entityKey r)) produtos

getCompraR :: ClienteId -> Handler Html
getCompraR cid = do
    (widget,_) <- generateFormPost (formVenda cid)
    msg <- getMessage
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
            
            <h1>
                Cadastro de compra

            <form method=post action=@{CompraR cid}>
                ^{widget}
                <input type="submit" value="comprar">
        |]

postCompraR :: ClienteId -> Handler Html
postCompraR cid = do
    ((result,_),_) <- runFormPost (formVenda cid)
    case result of
        FormSuccess venda -> do
            runDB $ insert venda
            setMessage [shamlet|
                <div>
                    compra incluido
            |]
            redirect (CarrinhoR cid)
        _ -> redirect HomeR

mult :: Double -> Double -> Double
mult = (*)

getCarrinhoR :: ClienteId -> Handler Html
getCarrinhoR cid = do
    let sql = "SELECT ??,??,?? FROM produto \
          \ INNER JOIN venda ON venda.prodid = produto.id \
          \ INNER JOIN cliente ON venda.cliid = cliente.id \
          \ WHERE cliente.id = ?"
    cliente <- runDB $ get404 cid
    tudo <- runDB $ rawSql sql [toPersistValue cid] :: Handler [(Entity Produto,Entity Venda,Entity Cliente)]
    defaultLayout $ do
        [whamlet|
            <h1>
                Carrinho de #{clienteNome cliente}
            <ul>
                $forall (Entity _ produto, Entity _ venda, Entity _ _) <- tudo
                    <li>
                        #{produtoNome produto}, #{mult (produtoPreco produto) (fromIntegral (vendaQt venda))} no dia #{show $ vendaDia venda}
        |]

