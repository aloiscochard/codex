module Codex.Project where

import Control.Exception (try, SomeException)
import Data.Functor
import Data.Maybe
import Data.String.Utils
import Data.Traversable (traverse)
import Distribution.InstalledPackageInfo
import Distribution.Hackage.DB (readHackage)
import Distribution.Hackage.Utils
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Package
import Distribution.Verbosity
import System.Directory
import System.FilePath

import qualified Data.List as List

import Codex.Internal

findPackageDescription :: FilePath -> IO (Maybe GenericPackageDescription)
findPackageDescription root = do
  files <- getDirectoryContents root
  traverse (readPackageDescription silent) $ List.find (endswith ".cabal") files

resolveCurrentProjectDependencies :: IO (PackageIdentifier, [PackageIdentifier])
resolveCurrentProjectDependencies = resolveProjectDependencies "."

resolveProjectDependencies :: FilePath -> IO (PackageIdentifier, [PackageIdentifier])
resolveProjectDependencies root = do
  pd <- maybe (error "No cabal file found.") id <$> findPackageDescription root
  xs <- either (fallback pd) return =<< resolveProjectInstalledDependencies root
  return (identifier pd, xs) where
    fallback pd e = do
      putStrLn $ concat ["cabal: ", show e]
      putStrLn "codex: *warning* falling back on dependency resolution using hackage"
      resolveWithHackage pd
    resolveWithHackage pd = do
      db <- readHackage
      return $ identifier <$> resolveDependencies db pd

resolveProjectInstalledDependencies :: FilePath -> IO (Either SomeException [PackageIdentifier])
resolveProjectInstalledDependencies root = try $ do
  lbi <- getPersistBuildConfig distPref
  let pkg   = localPkgDescr lbi
      ipkgs = installedPkgs lbi
      clbis = snd <$> allComponentsInBuildOrder lbi
      pkgs  = componentPackageDeps =<< clbis
      xs = fmap sourcePackageId $ (maybeToList . lookupInstalledPackageId ipkgs) =<< fmap fst pkgs
  return xs where
    distPref = joinPath [root, "dist"]
