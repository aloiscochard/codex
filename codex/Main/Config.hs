{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main.Config where

import Data.Yaml
import Distribution.Hackage.Utils (getHackagePath)
import GHC.Generics

import System.Directory
import System.FilePath

import Codex

import qualified Main.Config.Codex0 as C0

data ConfigState = Ready | TaggerNotFound

deriving instance Generic Codex
instance ToJSON Codex
instance FromJSON Codex

getConfigPath :: IO FilePath
getConfigPath = do
  homedir <- getHomeDirectory
  return $ homedir </> ".codex"

checkConfig :: Codex -> IO ConfigState
checkConfig cx = do
  taggerExe <- findExecutable tagger
  return $ case taggerExe of
    Just path -> Ready
    _         -> TaggerNotFound
  where
    tagger = head $ words (tagsCmd cx)

loadConfig :: IO Codex
loadConfig = decodeConfig >>= maybe defaultConfig return where
  defaultConfig = do
    hp <- getHackagePath
    let cx = Codex hp (taggerCmd Hasktags) True True
    encodeConfig cx
    return cx

encodeConfig :: Codex -> IO ()
encodeConfig cx = do
  path <- getConfigPath
  encodeFile path cx

decodeConfig :: IO (Maybe Codex)
decodeConfig = do
  path  <- getConfigPath
  cfg   <- config path
  case cfg of
    Nothing   -> do
      cfg0 <- config0 path
      case cfg0 of
        Nothing   -> return Nothing
        Just cfg0 -> do
            encodeConfig cfg
            return $ Just cfg
          where
            cfg = migrate cfg0
            migrate cx = Codex (C0.hackagePath cx) (C0.tagsCmd cx) True True
    cfg       -> return cfg
  where
    config :: FilePath -> IO (Maybe Codex)
    config = configOf
    config0 :: FilePath -> IO (Maybe C0.Codex)
    config0 = configOf
    configOf path = do
      res <- decodeFileEither path
      return $ eitherToMaybe res
    eitherToMaybe x = either (const Nothing) Just x

