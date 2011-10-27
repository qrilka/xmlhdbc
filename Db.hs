module Db where

import Database.HDBC
import Database.HDBC.PostgreSQL

import Data.Maybe
import Data.List
import qualified Data.Map as M

import Text.Printf

data Context = Context {                          
  ctxTables     :: M.Map String [String],
  ctxKeys       :: M.Map String [String]
  }

newContext :: Connection -> [(String, [String])] -> IO Context
newContext conn vk = do
  tables <- getTablesMeta conn
  keys <- getPrimaryKeys conn vk
  return Context{ ctxTables = tables, ctxKeys = keys}


getTablesMeta :: Connection -> IO (M.Map String [String])
getTablesMeta conn = do
  t <- getTables conn
  tdata <- colsOfTables conn t
  return $ M.fromList tdata

getPrimaryKeys :: Connection -> [(String, [String])] -> IO (M.Map String [String])
getPrimaryKeys c k = do
  let sql = unlines [ "select co.table_name,"
                    ,"  array_to_string(array(select column_name::text from information_schema.key_column_usage u"
                    ,"  where u.constraint_name = co.constraint_name), ' ')"
                    ,"  from information_schema.table_constraints co"
                    ,"  where constraint_type = 'PRIMARY KEY'" ]
  res <- quickQuery' c sql []
  let q = M.fromList (map row res ++ k)
  return q
    where row x = (fromSql (x!!0)::String, words (fromSql (x!!1) :: String))

columnNames :: [(String, SqlColDesc)] -> [String]
columnNames = map fst

tosql :: String -> SqlValue
tosql "\\NULL" = SqlNull
tosql x        = toSql x

xmlOfCol :: SqlValue -> String
xmlOfCol = fromSql

xmlOfRow :: [SqlValue] -> String
xmlOfRow = concatMap xmlOfCol

xmlOfSet :: [[SqlValue]] -> [String]
xmlOfSet = map xmlOfRow

xmlForestOf :: [String] -> String
xmlForestOf x = "xmlforest(" ++ intercalate "," (map coalesce x) ++ ")"
    where coalesce x' = printf "coalesce(%s::text,'') as %s" x' x' :: String

xmlRow :: String -> String -> String
xmlRow name x = "xmlelement(name " ++ name ++ ", " ++ x ++ ")"

xmlMakeSelect :: [String] -> String -> String
xmlMakeSelect columns table = 
    "select " ++ 
    xmlRow "row" (xmlForestOf columns) ++ 
    " from " ++ table

xmlMakeEqClauses:: [(String, String)] -> (String, [String])
xmlMakeEqClauses args = 
    let (cols, binds) = foldl chunk ([],[]) args
    in (intercalate " and " $ reverse cols, reverse binds)
    where chunk (c,b) (k,v) = (fmt k:c, v:b)
          fmt n = printf "%s = ?" n :: String

xmlMakeSelectQuery :: String -> [String] -> [(String, String)] -> (String, [String])
xmlMakeSelectQuery table cols args = 
    let sel = xmlMakeSelect cols table
    in let (whe, binds) = xmlMakeEqClauses args 
    in (sel ++ (if not (null whe) then " where " ++ whe else ""), binds)

colsOfTable :: Connection -> String -> IO (String, [String])
colsOfTable c t = do
    wtf <- fmap columnNames $ describeTable c t 
    return (t, wtf)

colsOfTables :: Connection -> [String] -> IO [(String, [String])]
colsOfTables c = mapM (colsOfTable c)

xmlDoSelect :: Connection -> String -> [String] -> IO String
xmlDoSelect conn query binds = do
    rs <- quickQuery' conn query (map tosql binds)
    return (intercalate "\n" $ t (xmlOfSet rs))
    where t x = ["<table>"] ++ x ++ ["</table>"]

doSelectFromTable :: Connection -> Context -> String -> [(String, String)] -> IO String
doSelectFromTable conn ctx tbl args = do
    let cols = fromMaybe ["null as __fake"] $ M.lookup tbl (ctxTables ctx) 
    let (sql, binds) = xmlMakeSelectQuery tbl cols args
    xmlDoSelect conn sql binds
