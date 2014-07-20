{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Window where

import Data.Word
import Control.Monad
import Control.Monad.IO.Class
import Graphics.XHB

import Lens
import Util
import Types hiding (geometry)


instance TypeConversion GetGeometryReply Geometry where
    convert r = Geometry (Position x' y') (Dimension w' h')
        where x' = fi $ x_GetGeometryReply r
              y' = fi $ y_GetGeometryReply r
              w' = fi $ width_GetGeometryReply r
              h' = fi $ height_GetGeometryReply r


setBorderColor :: MonadIO m => WindowId -> Word -> Z m ()
setBorderColor window bc = do
    let vp = toValueParam [(CWBorderPixel, fi bc)]
    connection $-> \c -> io $ changeWindowAttributes c window vp


setBorderWidth :: MonadIO m => WindowId -> Word -> Z m ()
setBorderWidth window bw = do
    let vp = toValueParam [(ConfigWindowBorderWidth, fi bw)]
    connection $-> \c -> io $ configureWindow c window vp


changeAttributes :: (MonadIO m, BitEnum e) => WindowId -> [(e, Word32)] -> Z m ()
changeAttributes window valueparams = connection $-> \c ->
    io $ changeWindowAttributes c window $ toValueParam valueparams


focus :: MonadIO m => WindowId -> Z m ()
focus window = do
    config . focusedBorderColor $-> setBorderColor window -- client
    let mk_setinputfocus = MkSetInputFocus InputFocusNone
                                           window
                                           (toValue TimeCurrentTime)
    connection $-> io . flip setInputFocus mk_setinputfocus


unfocus :: (Functor m, MonadIO m) => WindowId -> Z m ()
unfocus window = do
    config . normalBorderColor $-> setBorderColor window
    connection $-> io . getInputFocus >>= void . io . getReply


raise :: MonadIO m => WindowId -> Z m ()
raise = flip configure [(ConfigWindowStackMode, toValue StackModeAbove)]


lower :: MonadIO m => WindowId -> Z m ()
lower = flip configure [(ConfigWindowStackMode, toValue StackModeBelow)]


configure :: MonadIO m => WindowId -> [(ConfigWindow, Word32)] -> Z m ()
configure w vs = connection $-> \c -> io $ configureWindow c w $ toValueParam vs


attributes :: (MonadIO m, Functor m) => WindowId -> Z m (Receipt GetWindowAttributesReply)
attributes window = connection $-> io . flip getWindowAttributes window


geometry :: (MonadIO m, Functor m) => WindowId -> Z m (Receipt GetGeometryReply)
geometry window = connection $-> io . flip getGeometry (convertXid window)
