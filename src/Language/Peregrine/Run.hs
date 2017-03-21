{-# LANGUAGE QuasiQuotes #-}
module Language.Peregrine.Run where

import           Protocol.Backend.C.Base as CP

import           Data.String.Interpolate

import           Control.Concurrent
import           Control.Concurrent.Async
import qualified GHC.Conc as Conc

import           Control.Exception
import           System.Process.Typed
import           System.Directory
import           Development.Shake.FilePath

import           Language.Peregrine.DSL
import           Language.Peregrine.Compiler as Peregrine

import qualified Data.Map.Extensions as Map
import           Data.Map (Map)

import           Data.ByteString.Lazy (ByteString)

import           Utils
import           Data.List as List
import           Control.Arrow
import           Text.Read

opts :: CP.CompileOptions
opts = CP.CompileOptions
  { debug    = True
  , optLevel = 3
  , compiler = GCC
  , filename = "peregrine"
  }
 
type Pool a = Chan a
 
withPool :: Pool a -> (a -> IO b) -> IO b
withPool q io = bracket (readChan q) (writeChan q) io
 
mkPool :: [a] -> IO (Pool a)
mkPool as = do
  pool <- newChan
  writeList2Chan pool as
  return pool

-- Takes lines of the form "ABC     ,123.0"
-- and turns it into a map
getGroups :: String -> Map String Double
getGroups = Map.fromList . map go . lines
  where
    go line = case List.elemIndex ',' line of
      Just idx -> case readMaybe (drop (idx + 1) line) of
        Just d -> (take idx line, d)
        -- TODO handle "nan" (read expects "NaN")
        _ -> error $
          "Malformed input: got " ++ show line
      _ -> error $
        "Malformed input: Expected comma somewhere, got " ++ show line
 
-- Probably want to use a streaming primitive like conduit
runDirectory :: FilePath -> CP.Specification a -> Peregrine -> IO [(String, ByteString)]
runDirectory dir spec program = do
  CP.compile opts "bin" $ Peregrine.codegen spec program
  numCores <- Conc.getNumProcessors -- getNumCapabilities is so wrong
  pool <- mkPool (replicate numCores ())
  files <- filter ((".lz4"==) . takeExtension) <$> listDirectory dir
  timer "total" $ forConcurrently files $ \file -> do
    withPool pool $ \() -> do
      let path = dir </> file
      out <- timer file $ readProcessStdout_ $ shell
        [i|lz4 -d < ${path} | bin/peregrine | sort|]
      return (file, out)

