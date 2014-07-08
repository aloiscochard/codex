module Distribution.Utils where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (maybeToList)
import Data.Traversable (traverse)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import System.Directory
import System.FilePath

import qualified Data.List as List

identifier :: GenericPackageDescription -> PackageIdentifier
identifier = package . packageDescription

findPackageDescription :: FilePath -> IO (Maybe GenericPackageDescription)
findPackageDescription root = do
  files <- getDirectoryContents root
  traverse (readPackageDescription silent) $ fmap (\x -> root </> x) $ List.find (isSuffixOf ".cabal") files

findProjects :: FilePath -> IO [(FilePath, PackageIdentifier)]
findProjects fp = do
  root <- canonicalizePath fp
  xs <- listDirectory root
  ys <- traverse find xs
  return $ ys >>= maybeToList where
    find path = do
      isDirectory <- doesDirectoryExist path
      if isDirectory then readProject path else return Nothing
    listDirectory fp = do
      xs <- getDirectoryContents fp
      return . fmap (fp </>) $ filter (not . isPrefixOf ".") xs

readProject :: FilePath -> IO (Maybe (FilePath, PackageIdentifier))
readProject path = do
  pd <- findPackageDescription path
  return $ fmap (\x -> (path, identifier x)) pd

