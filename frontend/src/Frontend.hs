{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
        elAttr "h1" ("class" =: "title") $ text "AGENDE SEU EXAME"
        elAttr "div" ("class" =: "content") $ do
          elAttr "div" ("class" =: "banner") $ do
            elAttr "div" ("class" =: "banner-text") $ do
              el "p" $ text "A SUPER IMAGEM é mais praticidade para você. Profissionais capacitados, dias e horários flexíveis, equipamento de alta tecnologia e agendamento/resultados online. Além disso, atendemos a grande maioria dos convênios médicos."
              el "p" $ text "Tudo isso garante nossa excelência no atendimento e na qualidade dos exames."
            elAttr "div" ("class" =: "banner-image") $ do
              elAttr "img" ("src" =: "https://blog.sst.com.br/wp-content/uploads/2015/08/exames_medicos_ocupacionais-640x400.jpg") blank
          elAttr "div" ("class" =: "banner-reverse") $ do
            elAttr "div" ("class" =: "banner-text") $ do
              el "p" $ text "A SUPER IMAGEM acredita no atendimento de qualidade para todas as pessoas independente da classe social. Por isso, oferecemos a população o programa Social, que oferece valores diferenciados para pacientes com guia médica do SUS ou que possuam renda mensal de até 1 ½ salário mínimo por pessoa da família.  Entre em contato conosco e confira mais detalhes do programa."
            elAttr "div" ("class" =: "banner-image") $ do
              elAttr "img" ("src" =: "http://www.centroimagempi.com.br/wp-content/uploads/2016/09/exame-idosa.jpg") blank
  }