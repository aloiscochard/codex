{-# LANGUAGE CPP #-}
module Codex (Codex(..), defaultTagsFileName, Verbosity, module Codex) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif

import Control.Exception (try, SomeException)
import Control.Lens ((^.))
import Control.Lens.Review (bimap)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Machine
import Data.Maybe
import Data.List ((\\))
import Distribution.Package
import Distribution.Text
import Distribution.Verbosity
import Network.HTTP.Client (HttpException)
import System.Directory
import System.Directory.Machine (files, directoryWalk)
import System.FilePath
import System.Process

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import qualified Text.Printf as Printf

import Codex.Internal
import Codex.Project

-- TODO Replace the `Codex` context with a `Control.Reader.Monad `.

-- TODO Remove that function once using `Text` widely
replace :: String -> String -> String -> String
replace a b c = Text.unpack $ Text.replace (Text.pack a) (Text.pack b) (Text.pack c)

md5hash :: String -> String
md5hash = concatMap (Printf.printf "%02x") . C8.unpack . MD5.hash . C8.pack


data Tagging = Tagged | Untagged
  deriving (Eq, Show)

fromBool :: Bool -> Tagging
fromBool True = Tagged
fromBool False = Untagged

data Status = Source Tagging | Archive | Remote
  deriving (Eq, Show)

type Action = EitherT String IO

data Tagger = Ctags | Hasktags | HasktagsEmacs | HasktagsExtended
  deriving (Eq, Show, Read)

taggerCmd :: Tagger -> String
taggerCmd Ctags = "ctags --tag-relative=no --recurse -f \"$TAGS\" \"$SOURCES\""
taggerCmd Hasktags = "hasktags --ctags --output=\"$TAGS\" \"$SOURCES\""
taggerCmd HasktagsEmacs = "hasktags --etags --output=\"$TAGS\" \"$SOURCES\""
taggerCmd HasktagsExtended = "hasktags --ctags --extendedctag --output=\"$TAGS\" \"$SOURCES\""

taggerCmdRun :: Codex -> FilePath -> FilePath -> Action FilePath
taggerCmdRun cx sources tags' = do
  _ <- tryIO $ system command
  return tags' where
    command = replace "$SOURCES" sources $ replace "$TAGS" tags' $ tagsCmd cx

-- TODO It would be much better to work out which `Exception`s are thrown by which operations,
--      and store all of that in a ADT. For now, I'll just be lazy.
tryIO :: IO a -> Action a
tryIO io = do
  res <- liftIO $ (try :: IO a -> IO (Either SomeException a)) io
  either (left . show) return res

codexHash :: Codex -> String
codexHash cfg = md5hash $ show cfg

dependenciesHash :: [PackageIdentifier] -> String
dependenciesHash xs = md5hash $ xs >>= display

tagsFileHash :: Codex -> [PackageIdentifier] -> String -> String
tagsFileHash cx ds projectHash = md5hash $ concat [codexHash cx, dependenciesHash ds, projectHash]

computeCurrentProjectHash :: Codex -> IO String
computeCurrentProjectHash cx = if not $ currentProjectIncluded cx then return "*" else do
  xs <- runT $ (autoM getModificationTime) <~ (filtered p) <~ files <~ directoryWalk <~ source ["."]
  return . md5hash . show $ maximum xs
    where
      p fp = any (\f -> f fp) (fmap List.isSuffixOf extensions)
      extensions = [".hs", ".lhs", ".hsc"]

isUpdateRequired :: Codex -> [PackageIdentifier] -> String -> Action Bool
isUpdateRequired cx ds ph = do
  fileExist <- tryIO $ doesFileExist file
  if fileExist then do
    content <- tryIO $ TLIO.readFile file
    let hash = TextL.toStrict . TextL.drop 17 . head . drop 2 $ TextL.lines content
    return $ hash /= Text.pack (tagsFileHash cx ds ph)
  else
    return True
  where
    file = tagsFileName cx

status :: FilePath -> PackageIdentifier -> Action Status
status root i = do
  sourcesExist <- tryIO . doesDirectoryExist $ packageSources root i
  archiveExist <- tryIO . doesFileExist $ packageArchive root i
  case (sourcesExist, archiveExist) of
    (True, _) -> fmap (Source . fromBool) (liftIO . doesFileExist $ packageTags root i)
    (_, True) -> return Archive
    (_, _)    -> return Remote

fetch :: WS.Session -> FilePath -> PackageIdentifier -> Action FilePath
fetch s root i = do
  bs <- tryIO $ do
    createDirectoryIfMissing True (packagePath root i)
    openLazyURI s url
  either left write bs where
      write bs = fmap (const archivePath) $ tryIO $ BS.writeFile archivePath bs
      archivePath = packageArchive root i
      url = packageUrl i

openLazyURI :: WS.Session -> String -> IO (Either String BS.ByteString)
openLazyURI s = fmap (bimap showHttpEx (^. W.responseBody)) . try . WS.get s where
  showHttpEx :: HttpException -> String
  showHttpEx = show

extract :: FilePath -> PackageIdentifier -> Action FilePath
extract root i = fmap (const path) . tryIO $ read' path (packageArchive root i) where
  read' dir tar = Tar.unpack dir . Tar.read . GZip.decompress =<< BS.readFile tar
  path = packagePath root i

tags :: Builder -> Codex -> PackageIdentifier -> Action FilePath
tags bldr cx i = taggerCmdRun cx sources tags' where
    sources = packageSources hp i
    tags' = packageTags hp i
    hp = hackagePathOf bldr cx

assembly :: Builder -> Codex -> [PackageIdentifier] -> String -> [WorkspaceProject] -> FilePath -> Action FilePath
assembly bldr cx dependencies projectHash workspaceProjects o = do
  xs <- join . maybeToList <$> projects workspaceProjects
  tryIO $ mergeTags (fmap tags' dependencies ++ xs) o
  return o where
    projects [] = return Nothing
    projects xs = do
      tmp <- liftIO getTemporaryDirectory
      ys <- traverse (tags'' tmp) xs
      return $ Just ys where
        tags'' tmp (WorkspaceProject id' sources) = taggerCmdRun cx sources tags''' where
          tags''' = tmp </> concat [display id', ".tags"]
    mergeTags files' o' = do
      files'' <- filterM doesFileExist files'
      contents <- traverse TLIO.readFile files''
      case files' \\ files'' of
        [] -> return ()
        xs -> do
          putStrLn "codex: *warning* the following tags files where missings during assembly:"
          mapM_ putStrLn xs
          return ()
      let xs = concat $ fmap TextL.lines contents
          ys = if sorted then (Set.toList . Set.fromList) xs else xs
      TLIO.writeFile o' $ TextL.unlines (concat [headers, ys])
    tags' = packageTags $ hackagePathOf bldr cx
    headers = if tagsFileHeader cx then fmap TextL.pack [headerFormat, headerSorted, headerHash] else []
    headerFormat = "!_TAG_FILE_FORMAT\t2"
    headerSorted = concat ["!_TAG_FILE_SORTED\t", if sorted then "1" else "0"]
    headerHash = concat ["!_TAG_FILE_CODEX\t", tagsFileHash cx dependencies projectHash]
    sorted = tagsFileSorted cx
