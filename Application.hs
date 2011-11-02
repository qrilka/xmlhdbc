{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
module Application
       ( withXmlHDBC
       , withDevelAppPort
       ) where

import Yesod
import Yesod.Static
import Yesod.Default.Config (DefaultEnv)
import Yesod.Logger (Logger)
import Yesod.Default.Main (defaultRunner, defaultDevelApp)

import Data.Dynamic (Dynamic, toDyn)
import Data.Text (unpack)

import Data.Pool
import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

import Control.Exception.Control (onException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Arrow ((***))

import Db

data XmlHDBC = XmlHDBC {
  getStatic    :: Static
  , settings   :: AppConfig DefaultEnv
  , getLogger  :: Logger
  , getPool    :: Pool H.Connection
  , getContext :: Context
  }

staticFiles "static"

mkYesod "XmlHDBC" [parseRoutes|
/static StaticR Static getStatic
/ HomeR GET
/list/#String ListR GET
/create/#String CreateR POST
/delete/#String DeleteR POST
/update/#String UpdateR POST
|]

instance Yesod XmlHDBC where
  approot _ = ""


inDbPool :: Pool H.Connection -> (H.Connection -> IO a) -> IO a
inDbPool pool f =
  withPool' pool $ \conn -> do
    H.begin conn
    result <- onException (f conn) (liftIO $ H.rollback conn)
    H.commit conn
    return result

inXmlTxn :: (MonadIO m, Functor m) =>
            (H.Connection -> Context -> IO a) -> GGHandler sub XmlHDBC m a
inXmlTxn f = do
  XmlHDBC{getPool=pool, getContext=ctx} <- getYesod
  liftIO $ inDbPool pool $ \conn -> f conn ctx

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
  addScript $ StaticR jquery_form_js
  toWidget [lucius| form { display: inline }|]
  toWidget [julius|
function refresh() {
  $.get("@{ListR "test"}", function(data) {
    $('#table').empty();
    $(data).find('row').each(function() {
      var id = $(this).find('id').text();
      var name = $(this).find('name').text();
      $('#table').append($('<div id="row'+id+'"><form action="@{UpdateR "test"}" method="post">'+
        '<input type="hidden" name="id" value="'+id+'"/>'+
        '<input type="text" name="name" value="'+name+'"/><input type="submit" value="!"/></form>'+
        '<form action="@{DeleteR "test"}" method="post"><input value="X" type="submit" onclick="del('+id+')"/></form></div>'));
      $('#row'+id+' form').ajaxForm(function() {
        refresh();
        alert('done!');
      });
    });
  }, "xml");
}

function del(id) {
  $.post("@{DeleteR "test"}", {id: id}, refresh);
}

$(function() {
  $("#adder").ajaxForm(function() {
    alert("here you go!");
    refresh();
  });
  refresh();
});
|]
  [whamlet|
<p>Hello World!
<form #adder action=@{CreateR "test"} method=post>
  <input name=name type=text>
  <input type=submit>
<div #table>
|]

getListR :: String -> Handler RepXml
getListR table = do
  x <- inXmlTxn $ \conn ctx ->
    doSelectFromTable conn ctx table []
  return $ RepXml $ toContent x

type DbOp a = H.Connection -> Context -> String -> [(String, String)] -> IO a

postHandler :: String -> DbOp a -> Handler a
postHandler table f = do
  (params, _files) <- runRequestBody
  inXmlTxn $ \conn ctx -> do
    let unpacked = map (unpack *** unpack) params
        tableParams = filterParams ctx table unpacked
    f conn ctx table tableParams

postCreateR :: String -> Handler RepXml
postCreateR table = do
  postHandler table doInsertIntoTable
  return $ RepXml "<response/>"

postUpdateR :: String -> Handler RepXml
postUpdateR table = do
  postHandler table doUpdateTable
  return $ RepXml "<response/>"

postDeleteR :: String -> Handler RepXml
postDeleteR table = do
  postHandler table doDeleteFromTable
  return $ RepXml "<response/>"

withXmlHDBC :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withXmlHDBC conf logger f = do
  s <- static "static"
  let connect = H.connectPostgreSQL "dbname=rfid"
      disconnect = H.disconnect
  createPool connect disconnect 10 $ \p -> do
    ctx <- inDbPool p $ \conn -> newContext conn []
    let h = XmlHDBC s conf logger p ctx
    defaultRunner f h

withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withXmlHDBC
