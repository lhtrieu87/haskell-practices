module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)

main :: IO ()
main = do
  putStrLn "Hello world"
  printConfig
  printTime
  
printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

printTime = do
  time <- getCurrentTime
  print time
  

