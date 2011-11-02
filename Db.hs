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

tableKeys :: Context -> String -> [String]
tableKeys Context{ctxKeys=keys} table = fromMaybe [] $ M.lookup table keys

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

qparts :: String -> [(String, a)] -> (String, [a])
qparts delim vals = 
  let (s,b) = foldl (\(s',b') (k,v) -> ((k++" = ?"):s', v:b')) ([], []) vals 
  in (intercalate delim $ reverse s, reverse b)

xmlDoSelect :: Connection -> String -> [String] -> IO String
xmlDoSelect conn query binds = do
    rs <- quickQuery' conn query (map tosql binds)
    return (intercalate "\n" $ t (xmlOfSet rs))
    where t x = ["<table>"] ++ x ++ ["</table>"]

makeInsert :: String -> [(String, String)] -> (String, [String])
makeInsert table values =
    let cols = intercalate "," $ map fst values
    in let holders = intercalate "," $ map (const "?") values 
    in let binds = map snd values
    in ("insert into " ++ table ++ "(" ++ cols ++ ")"  ++ " values (" ++ holders ++ ")" , binds)

makeUpdate :: String -> [(String, String)] -> [String] -> (String, [String]) 
makeUpdate table values keys =
    let (keys', values') = partition (\(k,v) -> k `elem` keys) values
    in let (sets, binds)     = qparts ", " values'
    in let (clauses, binds') = qparts " and " keys'
    in ("update " ++ table ++ " set " 
                  ++ sets  ++ " where " 
                  ++ ((show $ length keys')::String)
                  ++ " > 0 and " ++ clauses, binds ++ binds')

makeDelete :: String -> [(String, String)] -> [String] -> (String, [String]) 
makeDelete table values keys =
    let (keys', _) = partition (\(k,_) -> k `elem` keys) values
    in let (clauses, binds') = qparts " and " keys'
    in ("delete from " ++ table ++ " where " ++ ((show $ length keys')::String)
                                ++ " > 0 and " ++ clauses, binds')
filterParams :: Context ->  String -> [(String, String)] -> [(String, String)]
filterParams ctx t p =
    let static = [ "OFFSET", "LIMIT", "GUID" ]
    in let cols = fromMaybe [] $ M.lookup t (ctxTables ctx)
    in filter (\(k,_v) -> k `elem` (static ++ cols)) p

doSelectFromTable :: Connection -> Context -> String -> [(String, String)] -> IO String
doSelectFromTable conn ctx tbl args = do
    let cols = fromMaybe ["null as __fake"] $ M.lookup tbl (ctxTables ctx) 
    let (sql, binds) = xmlMakeSelectQuery tbl cols args
    xmlDoSelect conn sql binds

doInsertIntoTable :: Connection -> Context -> String -> [(String, String)] -> IO ()
doInsertIntoTable conn _ctx table args = do
    let (sql, binds) = makeInsert table args
    _ <- quickQuery' conn sql (map tosql binds)
    return ()

doUpdateTable :: Connection -> Context -> String -> [(String, String)] -> IO ()
doUpdateTable conn ctx table values = do
    let keys = tableKeys ctx table
    let (sql, binds) = makeUpdate table values keys
    _ <- quickQuery' conn sql (map tosql binds)
    return ()

doDeleteFromTable :: Connection -> Context -> String -> [(String, String)] -> IO ()
doDeleteFromTable conn ctx table values = do
    let keys = tableKeys ctx table
    let (sql, binds) = makeDelete table values keys
    _ <- quickQuery' conn sql (map tosql binds)
    return ()
