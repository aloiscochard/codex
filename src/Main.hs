{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
import Control.Arrow
import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans.Either hiding (left, right)
import Data.Either
import Data.Traversable (traverse)
import Data.String.Utils
import Data.Yaml
import Distribution.Hackage.DB (readHackage)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Text
import Distribution.Verbosity
import GHC.Generics
import System.Directory
import System.Environment
import System.FilePath

import qualified Data.List as List

import Distribution.Hackage.Utils

import Codex

-- TODO Use a mergesort algorithm for `assembly`
-- TODO Better error handling and fine grained retry

retrying :: Int -> IO (Either a b) -> IO (Either [a] b)
retrying n x = retrying' n $ fmap (left (:[])) x where
  retrying' 0 x = x
  retrying' n x = retrying' (n - 1) $ x >>= \res -> case res of
    Left ls -> fmap (left (++ ls)) x
    Right r -> return $ Right r

getCurrentProject :: IO (Maybe GenericPackageDescription)
getCurrentProject = do
  files <- getDirectoryContents $ joinPath ["."]
  traverse (readPackageDescription silent) $ List.find (endswith ".cabal") files

tagsFile :: FilePath
tagsFile = joinPath ["codex.tags"]

cleanCache :: Codex -> IO ()
cleanCache cx = do
  xs <- listDirectory hp 
  ys <- fmap (rights) $ traverse (safe . listDirectory) xs
  zs <- traverse (safe . removeFile) . fmap (</> "tags") $ concat ys
  return () where
    hp = hackagePath cx
    safe = (try :: IO a -> IO (Either SomeException a))
    listDirectory fp = do
      xs <- getDirectoryContents fp 
      return . fmap (fp </>) $ filter (not . startswith ".") xs

update :: Codex -> Bool -> IO ()
update cx force = getCurrentProject >>= resolve where
  resolve Nothing = putStrLn "No cabal file found."
  resolve (Just project) = do
    dependencies <- fmap (\db -> resolveDependencies db project) readHackage
    traverse (putStrLn . show . identifier) dependencies
    shouldUpdate <- runEitherT . isUpdateRequired tagsFile $ fmap identifier dependencies
    when (either (const True) id shouldUpdate || force) $ do
      fileExist <- doesFileExist tagsFile
      when fileExist $ removeFile tagsFile 
      putStrLn $ concat ["Updating ", display . identifier $ project]
      results <- traverse (retrying 3 . runEitherT . getTags . identifier) dependencies 
      traverse (putStrLn . show) . concat $ lefts results
      generate dependencies where
        getTags i = status cx i >>= \x -> case x of
          (Source Tagged)   -> return ()
          (Source Untagged) -> tags cx i >>= (const $ getTags i)
          (Archive)         -> extract cx i >>= (const $ getTags i)
          (Remote)          -> fetch cx i >>= (const $ getTags i)
        generate xs = do 
          res <- runEitherT $ assembly cx (fmap identifier xs) tagsFile
          either (putStrLn . show) (const $ return ()) res

main :: IO ()
main = do
  cx    <- loadConfig
  args  <- getArgs
  run cx args where
    run cx ["cache", clean] = cleanCache cx
    run cx ["update"]             = update cx False
    run cx ["update", "--force"]  = update cx True
    run cx ["set", "tagger", "ctags"]     = encodeConfig $ cx { tagsCmd = taggerCmd Ctags }
    run cx ["set", "tagger", "hasktags"]  = encodeConfig $ cx { tagsCmd = taggerCmd Hasktags }
    run cx _          = putStrLn "Usage: codex [clean|update|set tagger [ctags|hasktags]]"

loadConfig :: IO Codex
loadConfig = decodeConfig >>= maybe defaultConfig return where
  defaultConfig = do
    hp <- getHackagePath
    let cx = Codex (taggerCmd Hasktags) hp
    encodeConfig cx
    return cx

deriving instance Generic Codex
instance ToJSON Codex
instance FromJSON Codex

encodeConfig :: Codex -> IO ()
encodeConfig cx = do
  path <- getConfigPath
  encodeFile path cx

decodeConfig :: IO (Maybe Codex)
decodeConfig = do
  path  <- getConfigPath
  res   <- decodeFileEither path
  return $ either (const Nothing) Just res 

getConfigPath :: IO FilePath
getConfigPath = do
  homedir <- getHomeDirectory
  return $ joinPath [homedir, ".codex"]
