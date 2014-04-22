{-# LANGUAGE CPP #-}
module Distribution.Hackage.Utils where

import Data.Maybe
import Distribution.Hackage.DB (Hackage)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Version
import System.Directory
import System.FilePath

import qualified Data.List as List
import qualified Data.Map as Map

-- TODO Remove once path extracted in hackage-db
getHackagePath :: IO FilePath
getHackagePath = do
 homedir <- getHomeDirectory
 return (joinPath [homedir,
#ifdef IS_DARWIN
    "Library", "Haskell", "repo-cache"
#else
    ".cabal", "packages"
#endif
    , "hackage.haskell.org"])

allDependencies :: GenericPackageDescription -> [Dependency]
allDependencies pd = concat [lds, eds, tds] where
  lds = condTreeConstraints =<< (maybeToList $ condLibrary pd) 
  eds = (condTreeConstraints . snd) =<< condExecutables pd 
  tds = (condTreeConstraints . snd) =<< condTestSuites pd 

resolveDependency :: Hackage -> Dependency -> Maybe GenericPackageDescription
resolveDependency db (Dependency (PackageName name) versionRange) = do
  pdsByVersion <- Map.lookup name db
  latest <- List.find (\x -> withinRange x versionRange) $ List.reverse $ List.sort $ Map.keys pdsByVersion
  Map.lookup latest pdsByVersion

resolveDependencies :: Hackage -> GenericPackageDescription -> [GenericPackageDescription]
resolveDependencies db pd = maybeToList . resolveDependency db =<< allDependencies pd 

