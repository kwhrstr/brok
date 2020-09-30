module Brok.Types.Brok
    ( Brok
    , appConfig
    , appTLSManager
    , mkApp
    ) where

import RIO
import Brok.Types.Config   (Config)
import Network.HTTP.Client (Manager)

data App = App
    { appConfig     :: Config
    , appTLSManager :: Manager
    }

mkApp :: Config -> Manager -> App
mkApp = App

type Brok a = RIO App a
