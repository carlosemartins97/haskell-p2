{-# LANGUAGE DeriveGeneric #-}
{-# language DeriveAnyClass  #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}

module Common.Api where



import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple

-- tipo que represnta a tabela
data Exame = Exame { cd_exame :: Int, nm_exame :: Text, vl_exame :: Double, qt_exame :: Int } deriving (Generic, ToJSON, FromJSON, ToRow, FromRow, Eq, Show)









--commonStuff :: String
--commonStuff = "Here is a string defined in Common.Api"