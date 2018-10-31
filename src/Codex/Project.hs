{-# LANGUAGE CPP #-}
module Codex.Project where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif

import Control.Exception (try, SomeException)
import Control.Monad (filterM)
import Data.Bool (bool)
import Data.Function
import Data.List (delete, isPrefixOf, union)
import Data.Maybe
import Distribution.InstalledPackageInfo
#if MIN_VERSION_hackage_db(2,0,0)
import Distribution.Hackage.DB (HackageDB, cabalFile, readTarball)
#else
import Distribution.Hackage.DB (Hackage, readHackage')
#endif
import Distribution.Package
import Distribution.PackageDescription
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
import Distribution.Sandbox.Utils (findSandbox)
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Verbosity
import Distribution.Version
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import Text.Read (readMaybe)

import qualified Data.List as List
import qualified Data.Map as Map
#if !MIN_VERSION_hackage_db(2,0,0)
import qualified Data.Version as Base
#endif

import Codex.Internal (Builder(..), stackListDependencies)

#if MIN_VERSION_hackage_db(2,0,0)
type Hackage = HackageDB
#endif

newtype Workspace = Workspace [WorkspaceProject]
  deriving (Eq, Show)

data WorkspaceProject = WorkspaceProject { workspaceProjectIdentifier :: PackageIdentifier, workspaceProjectPath :: FilePath }
  deriving (Eq, Show)

type ProjectDependencies = (Maybe PackageIdentifier, [PackageIdentifier], [WorkspaceProject])

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
  mpath <- findCabalFilePath root
  traverse (
#if MIN_VERSION_Cabal(2,2,0)
    readGenericPackageDescription
#else
    readPackageDescription
#endif
    silent) mpath

-- | Find a regular file ending with ".cabal" within a directory.
findCabalFilePath :: FilePath -> IO (Maybe FilePath)
findCabalFilePath path = do
  paths <- getDirectoryContents path
  case List.find ((&&) <$> dotCabal <*> visible) paths of
    Just p -> do
      let p' = path </> p
      bool Nothing (Just p') <$> doesFileExist p'
    Nothing -> pure Nothing
  where
    dotCabal = (".cabal" ==) . takeExtension
    visible  = not . List.isPrefixOf "."

resolveCurrentProjectDependencies :: Builder -> FilePath -> IO ProjectDependencies
resolveCurrentProjectDependencies bldr hackagePath = do
  mps <- localPackages
  case mps of
    Just ps -> resolveLocalDependencies bldr hackagePath ps
    Nothing -> do
      disableImplicitWorkspace <- isJust <$> lookupEnv "CODEX_DISABLE_WORKSPACE"
      ws <- if disableImplicitWorkspace
        then pure (Workspace [])
        else getWorkspace ".."
      resolveProjectDependencies bldr ws hackagePath "."
  where
    localPackages = do
      mpath <-
        case bldr of
          Cabal -> bool Nothing (Just ".") <$> doesFileExist "cabal.project"
          Stack _ -> pure (Just ".")
      case mpath of
        Nothing   -> pure Nothing
        Just path -> Just <$> findLocalPackages 2 path

-- | Resolve the dependencies of each local project package.
resolveLocalDependencies :: Builder -> FilePath -> [WorkspaceProject] -> IO ProjectDependencies
resolveLocalDependencies bldr hackagePath wps = do
  pids <- foldr mergeDependencies mempty <$> traverse resolve wps
  pure (Nothing, pids, wps)
  where
    resolve p@WorkspaceProject{workspaceProjectPath = packagePath} =
      let ws' = Workspace (delete p wps)
      in resolveProjectDependencies bldr ws' hackagePath packagePath
    mergeDependencies (_, pids, _) pids' =
      pids `union` pids'

-- TODO Optimize
resolveProjectDependencies :: Builder -> Workspace -> FilePath -> FilePath -> IO ProjectDependencies
resolveProjectDependencies bldr ws hackagePath root = do
  pd <- maybe (error "No cabal file found.") id <$> findPackageDescription root
  xs <- resolvePackageDependencies bldr hackagePath root pd
  ys <- resolveSandboxDependencies root
  let zs   = resolveWorkspaceDependencies ws pd
  let wsds = List.filter (shouldOverride xs) $ List.nubBy (on (==) prjId) $ concat [ys, zs]
  let pjds = List.filter (\x -> (((unPackageName . pkgName) x) /= "rts") && (List.notElem (pkgName x) $ fmap prjId wsds)) xs
  return (Just (identifier pd), pjds, wsds)
  where
    shouldOverride xs (WorkspaceProject x _) =
      maybe True (\y -> pkgVersion x >= pkgVersion y) $ List.find (\y -> pkgName x == pkgName y) xs
    prjId = pkgName . workspaceProjectIdentifier

resolveInstalledDependencies :: Builder -> FilePath -> GenericPackageDescription -> IO (Either SomeException [PackageIdentifier])
resolveInstalledDependencies bldr root pd = try $ do
  case bldr of
    Cabal -> do
      lbi <- withCabal
      let ipkgs = installedPkgs lbi
          clbis = allComponentsInBuildOrder' lbi
          pkgs  = componentPackageDeps =<< clbis
          ys = (maybeToList . lookupInstalledPackageId ipkgs) =<< fmap fst pkgs
          xs = fmap sourcePackageId $ ys
      return xs where
        withCabal = getPersistBuildConfig $ root </> "dist"
    Stack cmd ->
      filter (/= pid) <$> stackListDependencies cmd pname
  where
    pid = pd & packageDescription & package
    pname = pid & pkgName & unPackageName

allComponentsInBuildOrder' :: LocalBuildInfo -> [ComponentLocalBuildInfo]
allComponentsInBuildOrder' =
#if MIN_VERSION_Cabal(2,0,0)
  allComponentsInBuildOrder
#else
  fmap snd . allComponentsInBuildOrder
#endif

resolveHackageDependencies :: Hackage -> GenericPackageDescription -> [GenericPackageDescription]
resolveHackageDependencies db pd = maybeToList . resolveDependency db =<< allDependencies pd where
  resolveDependency _ (Dependency name versionRange) = do
    pdsByVersion <- lookupName name
    latest <- List.find (\x -> withinRange' x versionRange) $ List.reverse $ List.sort $ Map.keys pdsByVersion
    lookupVersion latest pdsByVersion
#if MIN_VERSION_hackage_db(2,0,0)
  lookupName name = Map.lookup name db
  lookupVersion latest pdsByVersion = cabalFile <$> Map.lookup latest pdsByVersion
#else
  lookupName name = Map.lookup (unPackageName name) db
  lookupVersion latest pdsByVersion = Map.lookup latest pdsByVersion
#endif

#if MIN_VERSION_hackage_db(2,0,0)
withinRange' :: Version -> VersionRange -> Bool
withinRange' = withinRange
#else
withinRange' :: Base.Version -> VersionRange -> Bool
withinRange' =
#if MIN_VERSION_Cabal(2,0,0)
  withinRange . mkVersion'
#else
  withinRange
#endif
#endif

resolvePackageDependencies :: Builder -> FilePath -> FilePath -> GenericPackageDescription -> IO [PackageIdentifier]
resolvePackageDependencies bldr hackagePath root pd = do
  xs <- either fallback return =<< resolveInstalledDependencies bldr root pd
  return xs where
    fallback e = do
      putStrLn $ concat ["codex: ", show e]
      putStrLn "codex: *warning* falling back on dependency resolution using hackage"
      resolveWithHackage
    resolveWithHackage = do
#if MIN_VERSION_hackage_db(2,0,0)
      db <- readTarball Nothing hackagePath
#else
      db <- readHackage' hackagePath
#endif
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
getWorkspace root =
  Workspace <$> findLocalPackages 1 root

-- | Recursively find local packages in @root@, up to @depth@ layers deep. The
-- @root@ directory has a depth of 0.
findLocalPackages :: Int -> FilePath -> IO [WorkspaceProject]
findLocalPackages depth root =
  catMaybes <$> go depth root
  where
    go n path
      | n < 0 = pure []
      | otherwise =
          (:) <$> readWorkspaceProject path
              <*> fmap mconcat (traverse (go (n - 1)) =<< listDirectories path)
    listDirectories path = do
      paths <- getDirectoryContents =<< canonicalizePath path
      filterM doesDirectoryExist ((path </>) <$> filter visible paths)
    visible path =
      (not . isPrefixOf ".") path && path `notElem` ["dist", "dist-new"]
