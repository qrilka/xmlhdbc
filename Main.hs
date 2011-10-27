{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
module Main where

import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)

import Application

main :: IO ()
main = defaultMain fromArgs withXmlHDBC