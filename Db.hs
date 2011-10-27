module Db where

import Database.HDBC.PostgreSQL

import Data.List
import qualified Data.Map as M


data Context = Context {                          
  tables     :: M.Map String [String],
  keys       :: M.Map String [String]
  }

newContext :: Connection -> IO Context
newContext = undefined

