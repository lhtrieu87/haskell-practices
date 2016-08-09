{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib2 
( runMyApp
)
where

import Control.Monad.Reader
import Control.Monad.State

newtype MyApp a = MyA {
  runA :: ReaderT AppConfig (StateT AppState IO) a
} deriving (Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
  let appConfig = AppConfig maxDepth
      appState  = AppState 0
  in runStateT (runReaderT (runA k) appConfig) appState

data AppConfig = AppConfig {
  cfgMaxDepth :: Int
} deriving (Show)

data AppState = AppState {
  stDeepestReached :: Int
} deriving (Show)
