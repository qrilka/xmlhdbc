module Auth where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Printf

import Database.HDBC
import Database.HDBC.PostgreSQL

import Control.Concurrent.STM

import Db

type Permission = String

data PermCheck = NoCheck | PermCheck [Permission] deriving(Show)

data PermCheckResult = HasPermission | NoPermission | NoSession

type Permissions = S.Set Permission

data SessionData =  SessionData { sdLogin :: String
                                , sdPermissions :: Permissions
                                }

type Sessions = M.Map String SessionData

type SessionStore = TVar Sessions

loadSessionData :: Connection -> [(String, String)] -> IO [(String, (String, [String]))] 
loadSessionData conn args = do
  let sql = "select guid, login, permission from vsessionpermission"
      (cl, binds) = foldl (\(w,a)(k,v) -> ((printf "%s = ?" k :: String):w, tosql v:a)) ([],[]) args
  res <- quickQuery' conn (sql ++ whereOf cl) binds
  return $ M.toList $ mk $ map (\x -> (valOf 0 x, valOf 1 x, valOf 2 x)) res
    where
          valOf :: Int -> [SqlValue] -> String
          valOf n = fromSql . (!!n)
          mk = foldl (\m (ssid, nm, pm) -> M.insertWith (\(n1, p1) (_n2, p2) -> (n1, p1 ++ p2)) ssid (nm, [pm]) m) M.empty
          whereOf [] = ""
          whereOf x  = " where " ++  intercalate " and " x

sessionsFromList :: [(String, (String, [String]))] -> Sessions
sessionsFromList x = 
    let d = map (\(ssid, (name, perm)) -> 
                  (ssid, SessionData { sdLogin = name, sdPermissions = S.fromList perm}) ) x
    in M.fromList d

addSessions :: TVar Sessions -> Sessions -> IO ()
addSessions store s = atomically $ do
        c  <- readTVar store
        writeTVar store $ M.union s c
        return ()

delSession :: String -> SessionStore-> IO ()
delSession s store = atomically $ do
  c  <- readTVar store
  writeTVar store $ M.delete s c
  return ()

hasPermission :: String -> [String] -> SessionStore -> IO PermCheckResult
hasPermission ssid pl store = do 
    sessions <- atomically $ readTVar store
    findP $ M.lookup ssid sessions
    where findP (Just SessionData{sdPermissions=ps}) =
            return $ if S.null $ S.intersection (S.fromList pl) ps
                     then NoPermission
                     else HasPermission

          findP Nothing = return NoSession 
