{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}

module XcbView where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Typeable
import Control.Monad.Trans (lift)
import Graphics.XHB as X

import Log
import Lens
import Util
import Types

data XcbView = XcbView
    deriving Typeable

type XcbViewStack = ViewStack

xcbView :: ViewComponent
xcbView = Component
    { componentId = "XcbView"
    , componentData = XcbView
    , execComponent = \f d -> f >> return d
    , onStartup = return . id
    , onShutdown = const $ return ()
    , someHandler = const $
        L.map SomeHandler [ ModelHandler modelPrinter ]
        ++
        L.map SomeHandler [ ClientConfigHandler clientConfigHandler ]
    }

modelPrinter :: Model -> XcbViewStack ()
modelPrinter = toLog . show

clientConfigHandler :: ClientConfigs -> XcbViewStack ()
clientConfigHandler configs = do
    -- model <- ask
    -- io . putStrLn $ "xcbView:\n" ++ show model
    mapM_ configure
        . M.toList . M.map (toValueParam . L.nub . L.map convertConfigs . S.toList) $ configs
    where
    configure (w, vp) = lift (askL connection) >>= \c -> do
        io $ putStrLn $ "configure " ++ show w ++ " with " ++ show vp
        io $ X.configureWindow c w vp

    convertConfigs = \case
        ConfigClientX      _ v _ -> (ConfigWindowX, fi v)
        ConfigClientY      _ v _ -> (ConfigWindowY, fi v)
        ConfigClientWidth  _ v _ -> (ConfigWindowWidth, fi v)
        ConfigClientHeight _ v _ -> (ConfigWindowHeight, fi v)
