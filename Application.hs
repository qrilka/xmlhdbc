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

import Data.Maybe
import Data.Dynamic (Dynamic, toDyn)
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Types (status401)

import Data.Pool
import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

import Control.Concurrent.STM.TVar

import Control.Exception.Control (onException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Arrow ((***))
import Control.Applicative ((<$>))

import Db
import Auth

import Debug.Trace

data XmlHDBC = XmlHDBC {
  getStatic     :: Static
  , getSessions :: SessionStore
  , getSettings :: AppConfig DefaultEnv
  , getLogger   :: Logger
  , getPool     :: Pool H.Connection
  , getContext  :: Context
  }

sessionCookieName :: B.ByteString
sessionCookieName = "xsid"
sessionTimeout :: Int
sessionTimeout = 5 * 60

staticFiles "static"

mkYesod "XmlHDBC" [parseRoutes|
/static StaticR Static getStatic
/ HomeR GET
/list/#String ListR GET
/create/#String CreateR POST
/delete/#String DeleteR POST
/update/#String UpdateR POST
/login LoginR POST
/logout LogoutR GET
|]

instance Yesod XmlHDBC where
  approot _ = ""


auth :: (MonadIO m, Functor m) => PermCheck -> GGHandler sub XmlHDBC m ()
auth NoCheck = return ()
auth (PermCheck ps) = do
  store <- getSessions <$> getYesod
  xsid <- lookupCookie (E.decodeUtf8 sessionCookieName)
  let checkPermission = maybe (return NoSession) (\x -> hasPermission (unpack x) ps store) xsid
  p <- liftIO $ checkPermission
  case p of
    NoSession     -> permissionDenied "boo"
    NoPermission  -> permissionDenied "forbidden"
    HasPermission -> return ()

perm o a = PermCheck ["*", o ++ "/" ++ a]

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
  $.get("@{ListR "test"}", function(data, status) {
    console.log(data, status);
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

function logout() {
  $.get("@{LogoutR}", refresh);
}

$(function() {
  $('#table').ajaxError(function() {
    $(this).empty();
  });
  $("#adder").ajaxForm(function() {
    alert("here you go!");
    refresh();
  });
  $("#login").ajaxForm(function() {
    refresh();
  });
  refresh();
});
|]
  [whamlet|
<p>Hello World!
<p>
  <form #login action=@{LoginR} method=post>
    <input type=text name=login>
    <input type=text name=password>
    <input type=submit value=login>
  <a href="javascript:logout()">Logout
<form #adder action=@{CreateR "test"} method=post>
  <input name=name type=text>
  <input type=submit>
<div #table>
|]

getListR :: String -> Handler RepXml
getListR table = do
  auth $ perm "list" table
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

postLoginR :: Handler RepXml
postLoginR = do
  login    <- unpack <$> fromMaybe "" <$> lookupPostParam "login"
  password <- unpack <$> fromMaybe "" <$> lookupPostParam "password"
  resp <- inXmlTxn $ \conn _ctx ->
    nullableStringOfRS <$> doSelectFunP conn "f_login" [login, password]
  withResult resp
    where withResult (Just s)  = do
            store <- getSessions <$> getYesod
--            liftIO $ infoM appLog $ "LOGGEED IN, XSID:" ++ s
            sd <- inXmlTxn $ \conn _ctx -> loadSessionData conn [("guid", s)]
            liftIO $ addSessions store $ sessionsFromList sd
            setCookie sessionTimeout sessionCookieName $ B.pack s
            return $ RepXml "<response/>"

          withResult Nothing = do
--            liftIO $ errorM appLog "LOGIN FAILED"
            sendResponseStatus status401 $ RepXml $ "<unauthorized/>"

getLogoutR :: Handler RepXml
getLogoutR = do
  cookie <- lookupCookie $ E.decodeUtf8 sessionCookieName
  case cookie of
    Just xsid -> do
      let xsid' = unpack xsid
          params = [("guid", xsid')]
      _ <- inXmlTxn $ \conn ctx -> trace ((show $ ctxTables ctx) ++ (show params)) doDeleteFromTable conn ctx "websession" params
      store <- getSessions <$> getYesod
      liftIO $ delSession xsid' store
      deleteCookie sessionCookieName
      return $ RepXml "<response/>"
    Nothing ->
      sendResponseStatus status401 $ RepXml $ "<unauthorized/>"

withXmlHDBC :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withXmlHDBC conf logger f = do
  s <- static "static"
  let connect = H.connectPostgreSQL "dbname=xmlhdbc"
      disconnect = H.disconnect
  createPool connect disconnect 10 $ \p -> do
    ctx <- inDbPool p $ \conn -> newContext conn []
    sd  <- inDbPool p $ \conn -> loadSessionData conn []
    sessions <- newTVarIO $ sessionsFromList sd
    let h = XmlHDBC s sessions conf logger p ctx
    defaultRunner f h

withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withXmlHDBC
