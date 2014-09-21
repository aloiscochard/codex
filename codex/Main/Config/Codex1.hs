{-# LANGUAGE DeriveGeneric #-}
module Main.Config.Codex1 where

import Data.Yaml
import GHC.Generics

data Codex = Codex { hackagePath :: FilePath, tagsCmd :: String, tagsFileHeader :: Bool, tagsFileSorted :: Bool }
  deriving Generic

instance ToJSON Codex
instance FromJSON Codex


