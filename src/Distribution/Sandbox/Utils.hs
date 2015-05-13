module Distribution.Sandbox.Utils where

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Maybe (mapMaybe, listToMaybe)

import Codex.Internal

findSandbox :: FilePath -> IO (Maybe FilePath)
findSandbox prjDir = do
  fileExists <- doesFileExist configFile
  if fileExists then readSandboxDir else return Nothing where
    readSandboxDir = do
      fileContent <- readFile configFile
      return $ removePrefixMany $ lines fileContent
    configFile = prjDir </> "cabal.sandbox.config"
    removePrefixMany = maybeFunctionMany $ removePrefix "prefix:"
    maybeFunctionMany :: (a -> Maybe b) -> [a] -> Maybe b
    maybeFunctionMany func list = listToMaybe $ mapMaybe func list

readSandboxSources :: FilePath -> IO [FilePath]
readSandboxSources sandboxPath = do
  fileExists  <- doesFileExist sourcesFile
  if fileExists then readSources else return [] where
    readSources = do
      fileContent <- readFile sourcesFile
      return $ projects fileContent where
        projects :: String -> [FilePath]
        projects x = sources x >>= (\x' -> fmap fst $ snd x')
        sources :: String -> [(String, [(FilePath, Int)])]
        sources x = read x
    sourcesFile = sandboxPath </> "add-source-timestamps"
