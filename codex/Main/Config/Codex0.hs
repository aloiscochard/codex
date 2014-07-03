{-# LANGUAGE DeriveGeneric #-}
module Main.Config.Codex0 where

import Data.Yaml
import GHC.Generics

data Codex = Codex { hackagePath :: FilePath, tagsCmd :: String }
  deriving Generic

instance ToJSON Codex
instance FromJSON Codex

