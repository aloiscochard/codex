{-# LANGUAGE DeriveGeneric #-}
module Main.Config.Codex1 where

import Data.Yaml
import GHC.Generics

import qualified Codex as New

data Codex = Codex { hackagePath :: FilePath, tagsCmd :: String, tagsFileHeader :: Bool, tagsFileSorted :: Bool }
  deriving Generic

instance ToJSON Codex
instance FromJSON Codex

migrateWarn :: IO ()
migrateWarn = do
  putStrLn "\tThe tags file will now include the tags of the *current* project as well,"
  putStrLn "\tif that is not the behavior you want, please edit `~/.codex`."

migrate :: Codex -> New.Codex
migrate cx = New.Codex
  True
  (hackagePath cx)
  New.defaultStackOpts
  (tagsCmd cx)
  (tagsFileHeader cx)
  (tagsFileSorted cx)
  New.defaultTagsFileName
