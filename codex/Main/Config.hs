{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main.Config where

import Data.Yaml
import GHC.Generics

import System.Directory
import System.FilePath

import Codex

import qualified Main.Config.Codex0 as C0
import qualified Main.Config.Codex1 as C1
import qualified Distribution.Hackage.DB as DB

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
    hp <- DB.hackagePath
    let cx = Codex True (dropFileName hp) (taggerCmd Hasktags) True True
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
      cfg1 <- config1 path
      case cfg1 of
        Nothing   -> do
          cfg0 <- config0 path
          case cfg0 of
            Nothing   -> return Nothing
            Just cfg0 -> do
                encodeConfig cfg
                warn
                return $ Just cfg
              where
                cfg = migrate cfg0
                migrate cx = Codex True (C0.hackagePath cx) (C0.tagsCmd cx) True True
        Just cfg1 -> do
            encodeConfig cfg
            warn
            return $ Just cfg
          where
            cfg = migrate cfg1
            migrate cx = Codex True (C1.hackagePath cx) (C1.tagsCmd cx) (C1.tagsFileHeader cx) (C1.tagsFileSorted cx)
    cfg       -> return cfg
  where
    warn = do
      putStrLn "codex: *warning* your configuration has been migrated automatically!\n"
      C1.warn
      putStrLn ""
    config :: FilePath -> IO (Maybe Codex)
    config = configOf
    config0 :: FilePath -> IO (Maybe C0.Codex)
    config0 = configOf
    config1 :: FilePath -> IO (Maybe C1.Codex)
    config1 = configOf
    configOf path = do
      res <- decodeFileEither path
      return $ eitherToMaybe res
    eitherToMaybe x = either (const Nothing) Just x

