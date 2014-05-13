module Codex.Project where

import Control.Exception (try, SomeException)
import Control.Monad (filterM)
import Data.Functor
import Data.Maybe
import Data.String.Utils
import Data.Traversable (traverse)
import Distribution.InstalledPackageInfo
import Distribution.Hackage.DB (Hackage, readHackage)
import Distribution.Hackage.Utils
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Package
import Distribution.Verbosity
import Distribution.Version
import System.Directory
import System.FilePath

import qualified Data.List as List
import qualified Data.Map as Map

newtype Workspace = Workspace [WorkspaceProject]
  deriving (Eq, Show)

data WorkspaceProject = WorkspaceProject PackageIdentifier FilePath
  deriving (Eq, Show)

type ProjectDependencies = (PackageIdentifier, [PackageIdentifier], [WorkspaceProject])

identifier :: GenericPackageDescription -> PackageIdentifier
identifier = package . packageDescription

allDependencies :: GenericPackageDescription -> [Dependency]
allDependencies pd = List.filter (not . isCurrent) $ concat [lds, eds, tds] where
  lds = condTreeConstraints =<< (maybeToList $ condLibrary pd) 
  eds = (condTreeConstraints . snd) =<< condExecutables pd 
  tds = (condTreeConstraints . snd) =<< condTestSuites pd 
  isCurrent (Dependency n _) = n == (pkgName $ identifier pd)

findPackageDescription :: FilePath -> IO (Maybe GenericPackageDescription)
findPackageDescription root = do
  files <- getDirectoryContents root
  traverse (readPackageDescription silent) $ fmap (\x -> joinPath [root, x]) $ List.find (endswith ".cabal") files

resolveCurrentProjectDependencies :: IO ProjectDependencies
resolveCurrentProjectDependencies = do
  ws <- getWorkspace ".."
  resolveProjectDependenciesWithWorkspace ws "."

-- TODO Optimize
resolveProjectDependenciesWithWorkspace :: Workspace -> FilePath -> IO ProjectDependencies
resolveProjectDependenciesWithWorkspace ws root = do
  pd <- maybe (error "No cabal file found.") id <$> findPackageDescription root
  xs <- resolveProjectDependencies root pd
  let wsds = List.filter (shouldOverride xs) $ resolveWorkspaceDependencies ws pd
  let pjds = List.filter (\x -> List.notElem (pkgName x) $ fmap (\(WorkspaceProject x _) -> pkgName x) wsds) xs
  return (identifier pd, pjds, wsds) where
    shouldOverride xs (WorkspaceProject x _) = 
      maybe True (\y -> pkgVersion x >= pkgVersion y) $ List.find (\y -> pkgName x == pkgName y) xs

resolveProjectDependencies :: FilePath -> GenericPackageDescription -> IO [PackageIdentifier]
resolveProjectDependencies root pd = do
  xs <- either (fallback pd) return =<< resolveInstalledDependencies root
  return xs where
    fallback pd e = do
      putStrLn $ concat ["cabal: ", show e]
      putStrLn "codex: *warning* falling back on dependency resolution using hackage"
      resolveWithHackage pd
    resolveWithHackage pd = do
      db <- readHackage
      return $ identifier <$> resolveHackageDependencies db pd

resolveInstalledDependencies :: FilePath -> IO (Either SomeException [PackageIdentifier])
resolveInstalledDependencies root = try $ do
  lbi <- getPersistBuildConfig distPref
  let pkg   = localPkgDescr lbi
      ipkgs = installedPkgs lbi
      clbis = snd <$> allComponentsInBuildOrder lbi
      pkgs  = componentPackageDeps =<< clbis
      xs = fmap sourcePackageId $ (maybeToList . lookupInstalledPackageId ipkgs) =<< fmap fst pkgs
  return xs where
    distPref = joinPath [root, "dist"]

resolveHackageDependencies :: Hackage -> GenericPackageDescription -> [GenericPackageDescription]
resolveHackageDependencies db pd = maybeToList . resolveDependency db =<< allDependencies pd where
  resolveDependency db (Dependency (PackageName name) versionRange) = do
    pdsByVersion <- Map.lookup name db
    latest <- List.find (\x -> withinRange x versionRange) $ List.reverse $ List.sort $ Map.keys pdsByVersion
    Map.lookup latest pdsByVersion

resolveWorkspaceDependencies :: Workspace -> GenericPackageDescription -> [WorkspaceProject]
resolveWorkspaceDependencies (Workspace ws) pd = maybeToList . resolveDependency =<< allDependencies pd where
  resolveDependency (Dependency name versionRange) = 
    List.find (\(WorkspaceProject (PackageIdentifier n v) _) -> n == name && withinRange v versionRange) ws

getWorkspace :: FilePath -> IO Workspace
getWorkspace _root = do
  root <- canonicalizePath _root
  xs <- listDirectory root 
  ys <- traverse find xs
  return . Workspace $ ys >>= maybeToList where
    find path = do
      pd <- findPackageDescription path
      return $ fmap (\x -> WorkspaceProject (identifier x) path) pd
    listDirectory fp = do
      xs <- getDirectoryContents fp 
      filterM doesDirectoryExist . fmap (fp </>) $ filter (not . startswith ".") xs
