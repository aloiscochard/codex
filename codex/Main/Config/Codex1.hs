{-# LANGUAGE DeriveGeneric #-}
module Main.Config.Codex1 where

import Data.Yaml
import GHC.Generics

data Codex = Codex { hackagePath :: FilePath, tagsCmd :: String, tagsFileHeader :: Bool, tagsFileSorted :: Bool }
  deriving Generic

instance ToJSON Codex
instance FromJSON Codex

warn :: IO ()
warn = do
  putStrLn "\tThe `codex.tags` will now include the tags of the *current* project as well,"
  putStrLn "\tif that is not the behavior you want, please edit `~/.codex`."

