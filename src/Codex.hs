module Codex (Codex(..), Verbosity, module Codex) where

import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Error
import Data.Hash.MD5
import Data.Traversable (traverse)
import Data.String.Utils
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Verbosity
import Network.Curl.Download.Lazy
import System.Directory
import System.FilePath
import System.Process

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.IO as TLIO

import Codex.Internal

-- TODO Replace the `Codex` context with a `Control.Reader.Monad `.

data Tagging = Tagged | Untagged
  deriving (Eq, Show)

fromBool :: Bool -> Tagging
fromBool True = Tagged
fromBool False = Untagged

data Status = Source Tagging | Archive | Remote
  deriving (Eq, Show)

type Action = ErrorT String IO

data Tagger = Ctags | Hasktags
  deriving (Eq, Show, Read)

taggerCmd :: Tagger -> String
taggerCmd Ctags = "ctags --tag-relative=no --recurse -f '$TAGS' '$SOURCES'"
taggerCmd Hasktags = "hasktags --ctags --output='$TAGS' '$SOURCES'"

-- TODO It would be much better to work out which `Exception`s are thrown by which operations,
--      and store all of that in a ADT. For now, I'll just be lazy.
tryIO :: IO a -> Action a
tryIO io = do
  res <- liftIO $ (try :: IO a -> IO (Either SomeException a)) io
  either (throwError . show) return res

dependenciesHash :: [PackageIdentifier] -> String
dependenciesHash xs = md5s . Str $ xs >>= display

isUpdateRequired :: FilePath -> [PackageIdentifier] -> Action Bool
isUpdateRequired file is = do
  fileExist <- tryIO $ doesFileExist file
  if fileExist then do
    content <- tryIO $ TLIO.readFile file
    let hash = TextL.toStrict . TextL.drop 13 . head . drop 2 $ TextL.lines content
    return $ hash /= (Text.pack $ dependenciesHash is)
  else 
    return True

status :: Codex -> PackageIdentifier -> Action Status
status cx i = do
  sourcesExist <- tryIO . doesDirectoryExist $ packageSources cx i
  archiveExist <- tryIO . doesFileExist $ packageArchive cx i
  case (sourcesExist, archiveExist) of
    (True, _) -> fmap (Source . fromBool) (liftIO . doesFileExist $ packageTags cx i)
    (_, True) -> return Archive
    (_, _)    -> return Remote

fetch :: Codex -> PackageIdentifier -> Action FilePath
fetch cx i = do
  bs <- tryIO $ do 
    createDirectoryIfMissing True (packagePath cx i)
    openLazyURI url
  either throwError write bs where
    write bs = fmap (const archivePath) $ tryIO $ BS.writeFile archivePath bs
    archivePath = packageArchive cx i
    url = packageUrl i

extract :: Codex -> PackageIdentifier -> Action FilePath
extract cx i = fmap (const path) . tryIO $ read path (packageArchive cx i) where
  read dir tar = Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar
  path = packagePath cx i

tags :: Codex -> PackageIdentifier -> Action FilePath
tags cx i = do
  tryIO . createProcess $ shell command
  return tags where
    sources = packageSources cx i
    tags = packageTags cx i
    command = replace "$SOURCES" sources $ replace "$TAGS" tags $ tagsCmd cx

assembly :: Codex -> [PackageIdentifier] -> FilePath -> Action FilePath
assembly cx is o = tryIO . fmap (const o) $ mergeTags (fmap tags is) o where
  mergeTags files o = do
    contents <- traverse TLIO.readFile files
    let xs = List.sort . concat $ fmap TextL.lines contents
    TLIO.writeFile o $ TextL.unlines (concat [headers, xs])
  tags i = packageTags cx i
  headers :: [TextL.Text]
  headers = fmap TextL.pack ["!_TAG_FILE_FORMAT 2", "!_TAG_FILE_SORTED 1", hash]
  hash = concat ["!_CODEX_HASH ", dependenciesHash is]

