{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Codex.Internal where

import Data.Char (isSpace)
import Data.Yaml
import Distribution.Package
import Distribution.Text
import GHC.Generics
import System.FilePath

import qualified Data.List as L

defaultTagsFileName :: FilePath
defaultTagsFileName = "codex.tags"

data Codex = Codex
  { currentProjectIncluded :: Bool
  , hackagePath :: FilePath
  , tagsCmd :: String
  , tagsFileHeader :: Bool
  , tagsFileSorted :: Bool
  , tagsFileName :: FilePath }
    deriving Show

deriving instance Generic Codex
instance ToJSON Codex
instance FromJSON Codex

packagePath :: Codex -> PackageIdentifier -> FilePath
packagePath cx i = hackagePath cx </> relativePath i where
  relativePath _ = name </> version where
    name = display $ pkgName i
    version = display $ pkgVersion i

packageArchive :: Codex -> PackageIdentifier -> FilePath
packageArchive cx i = packagePath cx i </> name where
  name = concat [display $ pkgName i, "-", display $ pkgVersion i, ".tar.gz"]

packageSources :: Codex -> PackageIdentifier -> FilePath
packageSources cx i = packagePath cx i </> name where
  name = concat [display $ pkgName i, "-", display $ pkgVersion i]

packageTags :: Codex -> PackageIdentifier -> FilePath
packageTags cx i = packagePath cx i </> "tags"

packageUrl :: PackageIdentifier -> String
packageUrl i = concat ["http://hackage.haskell.org/package/", path] where
  path = concat [name, "/", name, ".tar.gz"]
  name = concat [display $ pkgName i, "-", display $ pkgVersion i]

removePrefix :: String -> String -> Maybe String
removePrefix prefix str =
  if prefix `L.isPrefixOf` trim str
    then Just $ trim $ L.drop (length prefix) $ trim str
    else Nothing
 where
  trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
