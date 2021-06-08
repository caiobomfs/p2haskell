{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
-- import Network.HTTP.Types.Status
-- import Database.Persist.Postgresql

-- static/img/logo.png => img_logo_png
-- imagens baixadas
-- <img src=@{StaticR img_logo_png}> linha 24  also erro da imagem nao mudar e nao conseguir adicionar imagens novas
getPage1R :: Handler Html
getPage1R = do
    defaultLayout $ do

        toWidgetHead $(luciusFile "templates/page1.lucius")
        $(whamletFile "templates/page1.hamlet")

getPage2R :: Handler Html
getPage2R = do
    defaultLayout $ do
        
        toWidgetHead $(luciusFile "templates/page2.lucius")
        $(whamletFile "templates/page2.hamlet")

-- Monad Handler => Back-end
-- Monad Widget => Front-end
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do

        usuario <- lookupSession "_ID"
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")
              
-- aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa