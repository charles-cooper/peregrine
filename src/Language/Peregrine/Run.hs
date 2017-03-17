{-# LANGUAGE QuasiQuotes #-}
module Language.Peregrine.Run where

import           Protocol.Backend.C.Base as CP

import           Data.String.Interpolate

import           Control.Concurrent
import           Control.Concurrent.Async

import           Control.Exception
import           System.Process
import           System.Directory
import           Development.Shake.FilePath

import           Utils

import           Language.Peregrine.DSL
import           Language.Peregrine.Compiler as Peregrine

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
 
runDirectory :: FilePath -> CP.Specification a -> Peregrine -> IO ()
runDirectory dir spec program = do
  CP.compile opts "bin" $ Peregrine.codegen spec program
  pool <- mkPool (take 8{-num core-} (repeat ()))
  files <- filter ((".lz4"==) . takeExtension) <$> listDirectory dir
  timer "total" $ forConcurrently_ files $ \file -> do
    withPool pool $ \() -> do
      path <- pure $ dir </> file
      timer file $ callCommand [i|lz4 -d < ${path} | bin/peregrine | sort | tee foo > /dev/null|]

