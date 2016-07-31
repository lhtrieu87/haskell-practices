{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 3000 $ routes

routes :: ScottyM ()
routes = do
  get "/users" $ getUsers
  get "/users/:id" $ getUser
  post "/users" $ createUser

getUsers :: ActionM ()
getUsers = json allUsers

getUser :: ActionM ()
getUser = do
  id <- param "id"
  json (filter (matchesId id) allUsers)

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

createUser :: ActionM ()
createUser = do
  user <- jsonData :: ActionM User
  json user

data User = User { userId :: Int, userName :: String } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]
