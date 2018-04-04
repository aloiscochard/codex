{-# LANGUAGE CPP #-}
module Main.Config where

import Control.Exception (catch)
import Data.Yaml
import System.Directory
import System.FilePath

import Codex

import qualified Main.Config.Codex0 as C0
import qualified Main.Config.Codex1 as C1
import qualified Main.Config.Codex2 as C2
import qualified Main.Config.Codex3 as C3
import qualified Distribution.Hackage.DB as DB
import qualified Distribution.Hackage.DB.Errors as Errors

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
#if MIN_VERSION_hackage_db(2,0,0)
    hp <- DB.hackageTarball
      `catch` \Errors.NoHackageTarballFound -> do
        error $ unlines
          [ "couldn't find a Hackage tarball. This can happen if you use `stack` exclusively,"
          , "or just haven't run `cabal update` yet. To fix it, try running:"
          , ""
          , "    cabal update"
          ]
#else
    hp <- DB.hackagePath
#endif
    let cx = Codex True (dropFileName hp) defaultStackOpts (taggerCmd Hasktags) True True defaultTagsFileName
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
      cfg3 <- config3 path
      case cfg3 of
        Nothing -> do
          cfg2 <- config2 path
          case cfg2 of
            Nothing -> do
              cfg1 <- config1 path
              case cfg1 of
                Nothing -> config0 path
                cfg1'   -> return cfg1'
            cfg2' -> return cfg2'
        cfg3' -> return cfg3'
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
    config3 = reencodeConfigOf C3.migrate C3.migrateWarn

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

