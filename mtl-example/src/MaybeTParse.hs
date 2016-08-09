{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MaybeTParse 
( Parse  
, evalParse
) where

import MaybeT
import Control.Monad.State
import Date.Int (Int64)
import qualified Data.ByteString.Lazy as L

data ParseState = ParseState {
    string :: L.ByteString
  , offset :: Int64
} deriving (Show)

newtype Parse a = P {
  runP :: MaybeT (State ParseState) a 
} deriving (Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Maybe a
evalParse m s = evalState (runMaybeT (runP a)) (ParseState s 0)
