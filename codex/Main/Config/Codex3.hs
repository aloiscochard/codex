{-# LANGUAGE DeriveGeneric #-}
module Main.Config.Codex3 where

import Data.Yaml
import GHC.Generics

import qualified Codex as New

data Codex = Codex
  { currentProjectIncluded :: Bool
  , hackagePath :: FilePath
  , tagsCmd :: String
  , tagsFileHeader :: Bool
  , tagsFileSorted :: Bool
  , tagsFileName :: FilePath }
    deriving Generic

instance ToJSON Codex
instance FromJSON Codex

migrateWarn :: IO ()
migrateWarn = return ()

migrate :: Codex -> New.Codex
migrate cx =
  New.Codex
    True
    (hackagePath cx)
    (New.defaultStackOpts)
    (tagsCmd cx)
    (tagsFileHeader cx)
    (tagsFileSorted cx)
    (tagsFileName cx)

