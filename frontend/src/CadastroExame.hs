{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

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








-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank
  , _frontend_body = do
      
      elAttr "header" ("class" =: "header") $ do
          el "nav" $ do
            elAttr "a" ("href" =: "/" <> "class" =: "logo") $ do
              elAttr "span" ("class" =: "super") $ text "Super "
              elAttr "span" ("class" =: "imagem") $ text "IMAGEM"
            el "ul" $ do 
              el "li" $ do
                elAttr "a" ("href" =: "https://www.google.com.br") $ text "Ver exames"
              el "li" $ do
                elAttr "a" ("href" =: "https://www.google.com.br") $ text "Cadastrar exames"

      elAttr "main" ("class" =: "main") $ do
        elAttr "h1" ("class" =: "title") $ text "CADASTRE UM EXAME"
        
      elAttr "form" ("method" =: "POST" <> "action" =: "#" <> "class" =: "exame-form") $ do
        elAttr "div" ("class" =: "input-container") $ do
          divClass "input-group" $ do
            elAttr "label" ("class" =: "input-label" <> "for" =: "codigo") $ text "Código do exame"
            elAttr "input" ("type" =: "text" <> "id" =: "codigo" <> "name" =: "codigo") $ blank

          divClass "input-group" $ do
            elAttr "label" ("class" =: "input-label" <> "for" =: "exame") $ text "Nome do exame"
            elAttr "input" ("type" =: "text" <> "id" =: "exame" <> "name" =: "exame") $ blank

          divClass "input-group" $ do
            elAttr "label" ("class" =: "input-label" <> "for" =: "valor") $ text "Valor do exame"
            elAttr "input" ("type" =: "number" <> "id" =: "valor" <> "name" =: "valor") $ blank 
        elAttr "div" ("class" =: "btn-container") $ do
          elAttr "button" ("type" =: "submit" <> "class" =: "submit") $ text "Cadastrar"
          
          --FINALIZAR FORMULARIOS
          
  }
