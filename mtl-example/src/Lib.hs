module Lib where

import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

type App = ReaderT AppConfig (StateT AppState IO)

data AppConfig = AppConfig {
  cfgMaxDepth :: Int
} deriving (Show)

data AppState = AppState {
  stDeepestReached :: Int
} deriving (Show)


runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth = 
  let config = AppConfig maxDepth
      state  = AppState 0
  in runStateT (runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  unfilteredContents <- liftIO . getDirectoryContents $ path
  let contents = filter (\name -> name /= "." && name /= "..") unfilteredContents 
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $ 
          put st { stDeepestReached = newDepth }
        constrainedCount newDepth newPath
      else return []
  return $ (path, length contents) : concat rest
