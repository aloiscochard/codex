{-# LANGUAGE CPP #-}
module Codex.Project where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif

import Control.Exception (try, SomeException)
import Control.Monad
import Data.Function
import Data.Maybe
import Distribution.InstalledPackageInfo
import Distribution.Hackage.DB (Hackage, readHackage')
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Sandbox.Utils (findSandbox)
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Verbosity
import Distribution.Version
import System.Directory
import System.FilePath
import Text.Read (readMaybe)

import qualified Data.List as List
import qualified Data.Map as Map

import Codex.Internal (Builder(..), stackListDependencies)

newtype Workspace = Workspace [WorkspaceProject]
  deriving (Eq, Show)

data WorkspaceProject = WorkspaceProject { workspaceProjectIdentifier :: PackageIdentifier, workspaceProjectPath :: FilePath }
  deriving (Eq, Show)

type ProjectDependencies = (PackageIdentifier, [PackageIdentifier], [WorkspaceProject])

identifier :: GenericPackageDescription -> PackageIdentifier
identifier = package . packageDescription

allDependencies :: GenericPackageDescription -> [Dependency]
allDependencies pd = List.filter (not . isCurrent) $ concat [lds, eds, tds, bds] where
  lds = condTreeConstraints =<< (maybeToList $ condLibrary pd)
  eds = (condTreeConstraints . snd) =<< condExecutables pd
  tds = (condTreeConstraints . snd) =<< condTestSuites pd
  bds = (condTreeConstraints . snd) =<< condBenchmarks pd
  isCurrent (Dependency n _) = n == (pkgName $ identifier pd)

findPackageDescription :: FilePath -> IO (Maybe GenericPackageDescription)
findPackageDescription root = do
  contents  <- getDirectoryContents root
  files     <- filterM (doesFileExist . (</>) root) contents
  traverse (readPackageDescription silent) $ fmap (\x -> root </> x) $ List.find (List.isSuffixOf ".cabal") files

resolveCurrentProjectDependencies :: Builder -> FilePath -> IO ProjectDependencies
resolveCurrentProjectDependencies bldr hackagePath = do
  ws <- getWorkspace ".."
  resolveProjectDependencies bldr ws hackagePath "."

-- TODO Optimize
resolveProjectDependencies :: Builder -> Workspace -> FilePath -> FilePath -> IO ProjectDependencies
resolveProjectDependencies bldr ws hackagePath root = do
  pd <- maybe (error "No cabal file found.") id <$> findPackageDescription root
  xs <- resolvePackageDependencies bldr hackagePath root pd
  ys <- resolveSandboxDependencies root
  let zs   = resolveWorkspaceDependencies ws pd
  let wsds = List.filter (shouldOverride xs) $ List.nubBy (on (==) prjId) $ concat [ys, zs]
  let pjds = List.filter (\x -> (((unPackageName . pkgName) x) /= "rts") && (List.notElem (pkgName x) $ fmap prjId wsds)) xs
  return (identifier pd, pjds, wsds) where
    shouldOverride xs (WorkspaceProject x _) =
      maybe True (\y -> pkgVersion x >= pkgVersion y) $ List.find (\y -> pkgName x == pkgName y) xs
    prjId = pkgName . workspaceProjectIdentifier

resolveInstalledDependencies :: Builder -> FilePath -> GenericPackageDescription -> IO (Either SomeException [PackageIdentifier])
resolveInstalledDependencies bldr root pd = try $ do
  case bldr of
    Cabal -> do
      lbi <- withCabal
      let ipkgs = installedPkgs lbi
          clbis = snd <$> allComponentsInBuildOrder lbi
          pkgs  = componentPackageDeps =<< clbis
          ys = (maybeToList . lookupInstalledPackageId ipkgs) =<< fmap fst pkgs
          xs = fmap sourcePackageId $ ys
      return xs where
        withCabal = getPersistBuildConfig $ root </> "dist"
    Stack cmd -> let self = package (packageDescription pd)
             in  filter (/=self) <$> stackListDependencies cmd

resolveHackageDependencies :: Hackage -> GenericPackageDescription -> [GenericPackageDescription]
resolveHackageDependencies db pd = maybeToList . resolveDependency db =<< allDependencies pd where
  resolveDependency _ (Dependency (PackageName name) versionRange) = do
    pdsByVersion <- Map.lookup name db
    latest <- List.find (\x -> withinRange x versionRange) $ List.reverse $ List.sort $ Map.keys pdsByVersion
    Map.lookup latest pdsByVersion

resolvePackageDependencies :: Builder -> FilePath -> FilePath -> GenericPackageDescription -> IO [PackageIdentifier]
resolvePackageDependencies bldr hackagePath root pd = do
  xs <- either fallback return =<< resolveInstalledDependencies bldr root pd
  return xs where
    fallback e = do
      putStrLn $ concat ["codex: ", show e]
      putStrLn "codex: *warning* falling back on dependency resolution using hackage"
      resolveWithHackage
    resolveWithHackage = do
      db <- readHackage' hackagePath
      return $ identifier <$> resolveHackageDependencies db pd

resolveSandboxDependencies :: FilePath -> IO [WorkspaceProject]
resolveSandboxDependencies root =
  findSandbox root >>= maybe (return []) continue
 where
  continue cabalSandboxFolder = do
    fileExists  <- doesFileExist sourcesFile
    if fileExists then readSources else return []
   where
    sourcesFile = root </> cabalSandboxFolder </> "add-source-timestamps"
    readSources = do
      fileContent <- readFile sourcesFile
      xs <- traverse readWorkspaceProject $ projects fileContent
      return $ xs >>= maybeToList where
        projects :: String -> [FilePath]
        projects x = sources x >>= (\x' -> fst <$> snd x')
        sources :: String -> [(String, [(FilePath, Int)])]
        sources x = fromMaybe [] (readMaybe x)

resolveWorkspaceDependencies :: Workspace -> GenericPackageDescription -> [WorkspaceProject]
resolveWorkspaceDependencies (Workspace ws) pd = maybeToList . resolveDependency =<< allDependencies pd where
  resolveDependency (Dependency name versionRange) =
    List.find (\(WorkspaceProject (PackageIdentifier n v) _) -> n == name && withinRange v versionRange) ws

readWorkspaceProject :: FilePath -> IO (Maybe WorkspaceProject)
readWorkspaceProject path = do
  pd <- findPackageDescription path
  return $ fmap (\x -> WorkspaceProject (identifier x) path) pd

getWorkspace :: FilePath -> IO Workspace
getWorkspace _root = do
  root <- canonicalizePath _root
  xs <- listDirectory root
  ys <- traverse find xs
  return . Workspace $ ys >>= maybeToList where
    find path = do
      isDirectory <- doesDirectoryExist path
      if isDirectory then readWorkspaceProject path else return Nothing
    listDirectory fp = do
      xs <- getDirectoryContents fp
      return . fmap (fp </>) $ filter (not . List.isPrefixOf ".") xs
