{-# LANGUAGE TemplateHaskell #-}

module LaunchpadClient.Token where

import Prelude
import Database.Persist.TH
import qualified Web.Authenticate.OAuth as OA

data Token = Temporary OA.Credential | Access OA.Credential
    deriving (Show, Read, Eq)
derivePersistField "Token"
