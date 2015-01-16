module Distribution.Sandbox.Utils where

import Control.Applicative ((<$>))
import Data.Char (isSpace)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (readFile)
import Data.Maybe (mapMaybe, listToMaybe)

import qualified Data.List as L

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
        projects x = sources x >>= (\x -> fmap fst $ snd x)
        sources :: String -> [(String, [(FilePath, Int)])]
        sources x = read x
    sourcesFile = sandboxPath </> "add-source-timestamps"

removePrefix :: String -> String -> Maybe String
removePrefix prefix str =
  if prefix `L.isPrefixOf` trim str
    then Just $ trim $ L.drop (length prefix) $ trim str
    else Nothing
 where
  trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
