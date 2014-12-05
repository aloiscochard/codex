{-# LANGUAGE DeriveGeneric #-}
module Main.Config.Codex0 where

import Data.Yaml
import GHC.Generics

import qualified Codex as New

data Codex = Codex { hackagePath :: FilePath, tagsCmd :: String }
  deriving Generic

instance ToJSON Codex
instance FromJSON Codex

migrateWarn :: IO ()
migrateWarn = return ()

migrate :: Codex -> New.Codex
migrate cx = New.Codex True (hackagePath cx) (tagsCmd cx) True True
