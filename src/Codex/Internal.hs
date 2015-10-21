{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Codex.Internal where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Data.Char (isSpace)
import Data.Yaml
import Distribution.Package
import Distribution.Text
import GHC.Generics
import System.FilePath
import System.Process (shell, readCreateProcess)

import qualified Data.List as L

defaultTagsFileName :: FilePath
defaultTagsFileName = "codex.tags"

data Builder = Cabal | Stack

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

hackagePathOf :: Builder -> Codex -> FilePath
hackagePathOf Cabal cx = hackagePath cx
hackagePathOf Stack cx = hackagePath cx </> "packages"

packagePath :: FilePath -> PackageIdentifier -> FilePath
packagePath root i = root </> relativePath i where
  relativePath _ = name </> version where
    name = display $ pkgName i
    version = display $ pkgVersion i

packageArchive :: FilePath -> PackageIdentifier -> FilePath
packageArchive root i = packagePath root i </> name where
  name = concat [display $ pkgName i, "-", display $ pkgVersion i, ".tar.gz"]

packageSources :: FilePath -> PackageIdentifier -> FilePath
packageSources root i = packagePath root i </> name where
  name = concat [display $ pkgName i, "-", display $ pkgVersion i]

packageTags :: FilePath -> PackageIdentifier -> FilePath
packageTags root i = packagePath root i </> "tags"

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

readStackPath :: String -> IO String
readStackPath id' = init <$> readCreateProcess (shell ("stack path --" ++ id')) ""
