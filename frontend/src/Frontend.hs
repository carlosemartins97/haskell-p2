{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import Control.Monad
import Control.Monad.Fix
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)

import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static
import Text.Read
import Reflex.Dom.Core
import Data.Maybe
import Common.Api
import Common.Route
import Data.Aeson




getPath :: R BackendRoute -> T.Text
getPath p = renderBackendRoute checFullREnc p


sendRequest :: ToJSON a => R BackendRoute -> a -> XhrRequest T.Text
sendRequest r dados = postJson (getPath r) dados



reqExame :: ( DomBuilder t m , Prerender t m ) => m ()
reqExame = do

  divClass "input-container" $ do
      divClass "input-group" $ do
        elAttr "label" ("class" =: "input-label" <> "for" =: "nome") $ text "Nome do exame:"
        nm_exame <- inputElement def
        elAttr "label" ("class" =: "input-label" <> "for" =: "valor") $ text "valor do exame:"
        vl_exame <- numberInput
        elAttr "label" ("class" =: "input-label" <> "for" =: "qt") $ text "idade minima:"
        qt_exame <- numberInput
        let exam = fmap (\((n,v),q) -> Exame 0 n v q) (zipDyn (zipDyn (_inputElement_value nm_exame) vl_exame) qt_exame)
        (submitBtn,_) <- el' "button" (text "Inserir")
        let click = domEvent Click submitBtn
        let examEvt = tag (current exam) click
        _ :: Dynamic t (Event t (Maybe T.Text)) <- prerender
          (pure never)
          (fmap decodeXhrResponse <$>
            performRequestAsync (sendRequest (BackendRoute_Exame :/ ())
              <$> examEvt))
        return ()



numberInput :: (DomBuilder t m, Num a, Read a) => m (Dynamic t a)
numberInput = do
      n <- inputElement $ def
        & inputElementConfig_initialValue .~ "0"
        & inputElementConfig_elementConfig 
        . elementConfig_initialAttributes .~ ("type" =: "number")
      return $ fmap (fromMaybe 0 . readMaybe . T.unpack) 
                 (_inputElement_value n)


------------------------------------------------------------------------------------------------



getListReq :: XhrRequest ()
getListReq = xhrRequest "GET" (getPath (BackendRoute_Listar :/ ())) def


reqLista :: ( DomBuilder t m, Prerender t m, MonadHold t m, MonadFix m, PostBuild t m) => m ()
reqLista = do
  (btn, _) <- el' "button" (text "Listar")
  let click = domEvent Click btn
  exams :: Dynamic t (Event t (Maybe [Exame])) <- prerender
    (pure never)
    (fmap decodeXhrResponse <$> performRequestAsync (const getListReq <$> click))
  dynP <- foldDyn (\ps d -> case ps of
    Nothing -> []
    Just p -> d++p) [] (switchDyn exams)
  el "table" $ do
    el "thead" $ do
      el "tr" $ do
        el "th" (text "Id")
        el "th" (text "Nome")
        el "th" (text "Valor")
        el "th" (text "Idade mínima")
    el "tbody" $ do
      dyn_ (fmap sequence (ffor dynP (fmap tabExame)))




tabExame :: DomBuilder t m => Exame -> m ()
tabExame ex = do
  el "tr" $ do
    el "td" (text $ T.pack $ show $ cd_exame ex)
    el "td" (text $ nm_exame ex)
    el "td" (text $ T.pack $ show $ vl_exame ex)
    el "td" (text $ T.pack $ show $ qt_exame ex)






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
            el "div" $ do menuLi
      elAttr "div" ("class" =: "conteudo") $ do mainPag
      elAttr "script" ("src" =: $(static "main.js")) blank
  }

homePage :: (DomBuilder t m, PostBuild t m, MonadHold t m) => m ()
homePage = do
  elAttr "main" ("class" =: "main") $ do
    elAttr "h1" ("class" =: "title") $ text "AGENDAMENTOS"
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

formPage :: (DomBuilder t m, PostBuild t m, MonadHold t m, Prerender t m) => m ()
formPage = do
  elAttr "main" ("class" =: "main") $ do
    elAttr "h1" ("class" =: "title") $ text "CADASTRE SEU EXAME"
    reqExame -- chamada do input

examePage :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender t m) => m ()
examePage = do
  elAttr "main" ("class" =: "main") $ do
  elAttr "h1" ("class" =: "title") $ text "EXAMES CADASTRADOS"
  reqLista

data Pagina = Pagina1 | Pagina2 | Pagina3

clickLi :: DomBuilder t m => Pagina -> T.Text -> m (Event t Pagina)
clickLi p t = do
  (ev, _) <- el' "li" (elAttr "a" ("href" =: "#"  <> "id" =: (t)) (text t))
  return ((\_ -> p) <$> domEvent Click ev)

menuLi :: (DomBuilder t m, MonadHold t m) => m (Dynamic t Pagina)
menuLi = do
  evs <- elAttr "ul" ("class" =: "menulist") $ do
    p1 <- clickLi Pagina1 "Home"
    p2 <- clickLi Pagina2 "Cadastrar"
    p3 <- clickLi Pagina3 "Exames"
    return (leftmost [p1,p2,p3])
  holdDyn Pagina1 evs

currPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender t m) => Pagina -> m ()
currPag p =
  case p of
    Pagina1 -> homePage
    Pagina2 -> formPage
    Pagina3 -> examePage

mainPag :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, Prerender t m) => m ()
mainPag = do
  pag <- el "div" menuLi
  dyn_ $ currPag <$> pag