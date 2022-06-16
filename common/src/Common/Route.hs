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


import Data.Functor.Identity
import Data.Text (Text, unpack)
import Data.Function
import Obelisk.Route
import Obelisk.Route.TH



checFullREnc :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checFullREnc = checkEncoder fullRouteEncoder & 
    \case 
          Left err -> error $ unpack err
          Right encoder -> encoder


data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Exame  :: BackendRoute ()   -- rota exame
  BackendRoute_Listar :: BackendRoute ()




data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Exame -> PathSegment "exame" $ unitEncoder mempty
      BackendRoute_Listar -> PathSegment "listar" $ unitEncoder mempty
      
    ) --cliente é a rota
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)



concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
