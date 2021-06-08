{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Produto where

import Import

formProduto :: Form Produto
formProduto = renderDivs $ Produto
    <$> areq textField "Nome: " Nothing
    <*> areq doubleField "Preco: " Nothing

getProdutoR :: Handler Html
getProdutoR = do
    (widget,_) <- generateFormPost formProduto
    msg <- getMessage
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg
                <div>
                    ^{mensa}
                
            <h1>
                Cadastro de produto
            <form method=post action=@{ProdutoR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

postProdutoR :: Handler Html
postProdutoR = do
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess prod -> do
            runDB $ insert prod
            setMessage [shamlet|
                <div>
                    produto incluido
            |]
            redirect ProdutoR
        _ -> redirect HomeR
