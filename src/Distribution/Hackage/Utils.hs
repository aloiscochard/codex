{-# LANGUAGE CPP #-}
module Distribution.Hackage.Utils where

import System.Directory
import System.FilePath

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

