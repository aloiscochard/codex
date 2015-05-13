module Codex.Project where

import Control.Exception (try, SomeException)
import Data.Function
import Data.Maybe
import Data.String.Utils
import Distribution.InstalledPackageInfo
import Distribution.Hackage.DB (Hackage, readHackage)
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

import qualified Data.List as List
import qualified Data.Map as Map

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
  files <- getDirectoryContents root
  traverse (readPackageDescription silent) $ fmap (\x -> root </> x) $ List.find (endswith ".cabal") files

resolveCurrentProjectDependencies :: IO ProjectDependencies
resolveCurrentProjectDependencies = do
  ws <- getWorkspace ".."
  resolveProjectDependencies ws "."

-- TODO Optimize
resolveProjectDependencies :: Workspace -> FilePath -> IO ProjectDependencies
resolveProjectDependencies ws root = do
  pd <- maybe (error "No cabal file found.") id <$> findPackageDescription root
  xs <- resolvePackageDependencies root pd
  ys <- resolveSandboxDependencies root
  let zs   = resolveWorkspaceDependencies ws pd
  let wsds = List.filter (shouldOverride xs) $ List.nubBy (on (==) prjId) $ concat [ys, zs]
  let pjds = List.filter (\x -> List.notElem (pkgName x) $ fmap prjId wsds) xs
  return (identifier pd, pjds, wsds) where
    shouldOverride xs (WorkspaceProject x _) =
      maybe True (\y -> pkgVersion x >= pkgVersion y) $ List.find (\y -> pkgName x == pkgName y) xs
    prjId = pkgName . workspaceProjectIdentifier

resolveInstalledDependencies :: FilePath -> IO (Either SomeException [PackageIdentifier])
resolveInstalledDependencies root = try $ do
  lbi <- getPersistBuildConfig distPref
  let ipkgs = installedPkgs lbi
      clbis = snd <$> allComponentsInBuildOrder lbi
      pkgs  = componentPackageDeps =<< clbis
      ys = (maybeToList . lookupInstalledPackageId ipkgs) =<< fmap fst pkgs
      xs = fmap sourcePackageId $ ys
  return xs where
    distPref = root </> "dist"

resolveHackageDependencies :: Hackage -> GenericPackageDescription -> [GenericPackageDescription]
resolveHackageDependencies db pd = maybeToList . resolveDependency db =<< allDependencies pd where
  resolveDependency _ (Dependency (PackageName name) versionRange) = do
    pdsByVersion <- Map.lookup name db
    latest <- List.find (\x -> withinRange x versionRange) $ List.reverse $ List.sort $ Map.keys pdsByVersion
    Map.lookup latest pdsByVersion

resolvePackageDependencies :: FilePath -> GenericPackageDescription -> IO [PackageIdentifier]
resolvePackageDependencies root pd = do
  xs <- either (fallback pd) return =<< resolveInstalledDependencies root
  return xs where
    fallback pd' e = do
      putStrLn $ concat ["cabal: ", show e]
      putStrLn "codex: *warning* falling back on dependency resolution using hackage"
      resolveWithHackage pd'
    resolveWithHackage pd' = do
      db <- readHackage
      return $ identifier <$> resolveHackageDependencies db pd'

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
        sources x = read x

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
      return . fmap (fp </>) $ filter (not . startswith ".") xs
