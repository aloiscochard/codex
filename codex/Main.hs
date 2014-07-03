import Control.Arrow
import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Trans.Either hiding (left, right)
import Data.Either
import Data.Functor
import Data.String.Utils
import Data.Traversable (traverse)
import Distribution.Text
import Paths_codex (version)
import System.Directory
import System.Environment
import System.FilePath
import System.Exit

import Codex
import Codex.Project
import Main.Config

-- TODO Add 'cache dump' to dump all tags in stdout (usecase: pipe to grep)
-- TODO Use a mergesort algorithm for `assembly`
-- TODO Better error handling and fine grained retry

retrying :: Int -> IO (Either a b) -> IO (Either [a] b)
retrying n x = retrying' n $ fmap (left (:[])) x where
  retrying' 0 x = x
  retrying' n x = retrying' (n - 1) $ x >>= \res -> case res of
    Left ls -> fmap (left (++ ls)) x
    Right r -> return $ Right r

tagsFile :: FilePath
tagsFile = joinPath ["codex.tags"]

cleanCache :: Codex -> IO ()
cleanCache cx = do
  xs <- listDirectory hp
  ys <- fmap (rights) $ traverse (safe . listDirectory) xs
  zs <- traverse (safe . removeFile) . fmap (</> "tags") $ concat ys
  return () where
    hp = hackagePath cx
    safe = (try :: IO a -> IO (Either SomeException a))
    listDirectory fp = do
      xs <- getDirectoryContents fp
      return . fmap (fp </>) $ filter (not . startswith ".") xs

update :: Codex -> Bool -> IO ()
update cx force = do
  (project, dependencies, workspaceProjects) <- resolveCurrentProjectDependencies

  shouldUpdate <-
    if (null workspaceProjects) then
      either (const True) id <$> (runEitherT $ isUpdateRequired tagsFile dependencies)
    else return True

  if (shouldUpdate || force) then do
    fileExist <- doesFileExist tagsFile
    when fileExist $ removeFile tagsFile
    putStrLn $ concat ["Updating ", display project]
    results <- traverse (retrying 3 . runEitherT . getTags) dependencies
    traverse (putStrLn . show) . concat $ lefts results
    res <- runEitherT $ assembly cx dependencies workspaceProjects tagsFile
    either (putStrLn . show) (const $ return ()) res
  else
    putStrLn "Nothing to update."
  where
    getTags i = status cx i >>= \x -> case x of
      (Source Tagged)   -> return ()
      (Source Untagged) -> tags cx i >>= (const $ getTags i)
      (Archive)         -> extract cx i >>= (const $ getTags i)
      (Remote)          -> fetch cx i >>= (const $ getTags i)

help :: IO ()
help = putStrLn $
  unlines [ "Usage: codex [update] [cache clean] [set tagger [hasktags|ctags]] [set format [vim|emacs]]"
          , "             [--help]"
          , "             [--version]"
          , ""
          , " update                Synchronize the `codex.tags` file in the current cabal project directory"
          , " update --force        Discard `codex.tags` file hash and force regeneration"
          , " cache clean           Remove all `tags` file from the local hackage cache]"
          , " set tagger <tagger>   Update the `~/.codex` configuration file for the given tagger (hasktags|ctags)."
          , " set format <format>   Update the `~/.codex` configuration file for the given format (vim|emacs)."
          , ""
          , "By default `hasktags` will be used, and need to be in the `PATH`, the tagger command can be fully customized in `~/.codex`."
          , ""
          , "Note: codex will browse the parent directory for cabal projects and use them as dependency over hackage when possible." ]

main :: IO ()
main = do
  cx    <- loadConfig
  args  <- getArgs
  run cx args where
    run cx ["cache", clean] = cleanCache cx
    run cx ["update"]             = withConfig cx (\x -> update x False)
    run cx ["update", "--force"]  = withConfig cx (\x -> update x True)
    run cx ["set", "tagger", "ctags"]     = encodeConfig $ cx { tagsCmd = taggerCmd Ctags }
    run cx ["set", "tagger", "hasktags"]  = encodeConfig $ cx { tagsCmd = taggerCmd Hasktags }
    run cx ["set", "format", "emacs"]     = encodeConfig $ cx { tagsFileHeader = False, tagsFileSorted = False }
    run cx ["set", "format", "vim"]       = encodeConfig $ cx { tagsFileHeader = True, tagsFileSorted = True }
    run cx ["--version"] = putStrLn $ concat ["codex: ", display version]
    run cx ["--help"] = help
    run cx []         = help
    run cx (x:_)      = fail $ concat ["codex: '", x,"' is not a codex command. See 'codex --help'."]

    withConfig cx f = checkConfig cx >>= \state -> case state of
      TaggerNotFound  -> fail $ "codex: tagger not found."
      Ready           -> f cx

    fail msg = do
      putStrLn $ msg
      exitWith (ExitFailure 1)

