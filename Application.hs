{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, OverloadedStrings #-}
module Application    
       ( withXmlHDBC
       , withDevelAppPort
       ) where

import Yesod
import Yesod.Default.Config (DefaultEnv)
import Yesod.Logger (Logger)
import Yesod.Default.Main (defaultRunner, defaultDevelApp)

import Data.Dynamic (Dynamic, toDyn)
import Data.Text (Text, unpack)

import Data.Pool
import qualified Database.HDBC as H
import qualified Database.HDBC.PostgreSQL as H

import Control.Exception.Control (onException)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Db


data XmlHDBC = XmlHDBC {
  settings     :: AppConfig DefaultEnv
  , getLogger  :: Logger
  , getPool    :: Pool H.Connection
  , getContext :: Context
  }

mkYesod "XmlHDBC" [parseRoutes|
/ HomeR GET
/list/#Text ListTableR GET
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
getHomeR = defaultLayout [whamlet|Hello World!|]

getListTableR :: Text -> Handler RepXml
getListTableR table = do
  x <- inXmlTxn $ \conn ctx ->
    doSelectFromTable conn ctx (unpack table) []
  return $ RepXml $ toContent x --("<foo>" ++ (show x) ++ "</foo>")

withXmlHDBC :: AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ()
withXmlHDBC conf logger f = do
  let connect = H.connectPostgreSQL "dbname=rfid" 
      disconnect = H.disconnect
  createPool connect disconnect 10 $ \p -> do
    ctx <- inDbPool p $ \conn -> newContext conn []
    let h = XmlHDBC conf logger p ctx
    defaultRunner f h

withDevelAppPort :: Dynamic
withDevelAppPort = toDyn $ defaultDevelApp withXmlHDBC
