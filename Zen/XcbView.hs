{-# LANGUAGE LambdaCase #-}

module XcbView where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Control.Monad.State
import Control.Monad.Reader
import Graphics.XHB as X hiding (Setup)

import Lens
import Util
import Model
import Types


xcbView :: View
xcbView configs = do
    model <- ask
    io . putStrLn $ "xcbView:\n" ++ show model
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
