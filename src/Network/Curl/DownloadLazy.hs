-- TODO Remove once https://github.com/GaloisInc/curl/pull/11 is released.

--------------------------------------------------------------------
-- |
-- Module    : Network.Curl.DownloadLazy
-- Copyright : (c) Don Stewart
-- License   : BSD3
--
-- Stability :  provisional
-- Portability: posix
--
-- This module is equivalent to download-curl's Network.Curl.Download.Lazy
-- A binding to curl, an efficient, high level library for
-- retrieving files using Uniform Resource Locators (URLs).
--
-- Content may be retrieved as a lazy "ByteString".
--
-- Error handling is encapsulated in the "Either" type.
--
--------------------------------------------------------------------

module Network.Curl.DownloadLazy (
        -- * The basic lazy interface to network content
          openLazyURI
        , openLazyURIWithOpts

    ) where

import           Foreign
import           Network.Curl

import           Data.IORef

import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

------------------------------------------------------------------------

-- | Download content specified by a url using curl, returning the
-- content as a lazy "ByteString".
--
-- If an error occurs, "Left" is returned, with a
-- protocol-specific error string.
--
-- Examples:
--
-- > openLazyURI "http://haskell.org"
--
openLazyURI :: String -> IO (Either String L.ByteString)
openLazyURI = openLazyURIWithOpts []

-- | Like openURI, but takes curl options.
--
-- Examples:
--
-- > openLazyURIWithOpts [CurlPost True] "http://haskell.org"
--
openLazyURIWithOpts :: [CurlOption] -> String -> IO (Either String L.ByteString)
openLazyURIWithOpts opts s = case parseURL s of
     Nothing  -> return $ Left $ "Malformed url: "++ s
     Just url -> do
        e <- getFile url opts
        return $ case e of
             Left err   -> Left $ "Failed to connect: " ++ err
             Right src  -> Right src

------------------------------------------------------------------------
-- Internal:
--

newtype URL = URL String

parseURL :: String -> Maybe URL
parseURL s = Just (URL s) -- no parsing

getFile :: URL -> [CurlOption] -> IO (Either String L.ByteString)
getFile (URL url) flags = do
    h   <- initialize
    ref <- newIORef L.Empty

    _ <- setopt h (CurlFailOnError True)
    setDefaultSSLOpts h url
    _ <- setopt h (CurlURL url)
    _ <- setopt h (CurlWriteFunction (gather ref))
    mapM_ (setopt h) flags
    rc         <- perform h
    chunks     <- readIORef ref

    return $ if rc /= CurlOK
        then Left (show rc)
        else Right $! revSpine chunks

--          fp <- newForeignPtr finalizerFree buf'
--          return (Right $! S.fromForeignPtr fp 0 (fromIntegral sz))

gather :: IORef L.ByteString -> WriteFunction
gather r = writer $ \chunk -> do
    chunks <- readIORef r
    let chunks' = L.Chunk chunk chunks
    writeIORef r $! chunks'

-- memcpy chunks of data into our bytestring.
writer :: (S.ByteString -> IO ()) -> WriteFunction
writer f src sz nelems _ = do
    let n' = sz * nelems
    f =<< S.create (fromIntegral n') (\dest -> S.memcpy dest (castPtr src) (fromIntegral n'))
    return n'


-- reverse just the spine of a lazy bytestring
revSpine :: L.ByteString -> L.ByteString
revSpine l =  rev l L.Empty
  where
    rev L.Empty a        = a
    rev (L.Chunk x xs) a = rev xs (L.Chunk x a)
