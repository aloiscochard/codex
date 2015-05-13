module Main.Config where

import Data.Yaml
import System.Directory
import System.FilePath

import Codex

import qualified Main.Config.Codex0 as C0
import qualified Main.Config.Codex1 as C1
import qualified Main.Config.Codex2 as C2
import qualified Distribution.Hackage.DB as DB

data ConfigState = Ready | TaggerNotFound

getConfigPath :: IO FilePath
getConfigPath = do
  homedir <- getHomeDirectory
  return $ homedir </> ".codex"

checkConfig :: Codex -> IO ConfigState
checkConfig cx = do
  taggerExe <- findExecutable tagger
  return $ case taggerExe of
    Just _    -> Ready
    _         -> TaggerNotFound
  where
    tagger = head $ words (tagsCmd cx)

loadConfig :: IO Codex
loadConfig = decodeConfig >>= maybe defaultConfig return where
  defaultConfig = do
    hp <- DB.hackagePath
    let cx = Codex True (dropFileName hp) (taggerCmd Hasktags) True True defaultTagsFileName
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
      cfg2 <- config2 path
      case cfg2 of
        Nothing -> do
          cfg1 <- config1 path
          case cfg1 of
            Nothing -> config0 path
            cfg1'   -> return cfg1'
        cfg2' -> return cfg2'
    cfg'      -> return cfg'
  where
    warn :: IO () -> IO ()
    warn migrateWarn = do
      putStrLn "codex: *warning* your configuration has been migrated automatically!\n"
      migrateWarn
      putStrLn ""
    config  = configOf
    config0 = reencodeConfigOf C0.migrate C0.migrateWarn
    config1 = reencodeConfigOf C1.migrate C1.migrateWarn
    config2 = reencodeConfigOf C2.migrate C2.migrateWarn

    reencodeConfigOf migrate migrateWarn path = do
      rawCfg <- configOf path
      let cfg = fmap migrate rawCfg
      case cfg of
        Nothing   -> return ()
        Just cfg' -> do
          encodeConfig cfg'
          warn migrateWarn
      return cfg

    configOf path = do
      res <- decodeFileEither path
      return $ eitherToMaybe res

    eitherToMaybe x = either (const Nothing) Just x

