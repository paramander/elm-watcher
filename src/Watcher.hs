{-# LANGUAGE OverloadedStrings #-}
module Watcher where

import System.Directory (getCurrentDirectory)
import System.FilePath.Posix
import qualified System.FSNotify as Notify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

import Compile (compile)

data WatchConfig = WatchConfig
    { watchDir :: FilePath
    , compileFile :: FilePath
    , outputDir :: FilePath
    }

watchWithConfig :: WatchConfig -> IO ()
watchWithConfig cnf = do
  Notify.withManager $ \notifyManager -> do
    Notify.watchTree notifyManager (watchDir cnf) (const True) $ \_ -> do
      compileRes <- compile file output
      case compileRes of
        Left err -> do
          putStrLn $ "error compiling " ++ file ++ ": " ++ err
        Right _ -> do
          putStrLn $ "detected Elm file change"
    forever $ threadDelay 100000
  where
    file = (watchDir cnf) </> (compileFile cnf)
    output = (outputDir cnf)
