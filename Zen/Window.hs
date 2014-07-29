{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Window where

import Data.Word
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Graphics.XHB hiding (Setup)

import Lens
import Util
import Types hiding (geometry)


instance TypeConversion GetGeometryReply Geometry where
    convert r = Geometry (Position x' y') (Dimension w' h')
        where x' = fi $ x_GetGeometryReply r
              y' = fi $ y_GetGeometryReply r
              w' = fi $ width_GetGeometryReply r
              h' = fi $ height_GetGeometryReply r


setBorderColor :: (MonadReader Setup m, MonadIO m) => WindowId -> Word -> m ()
setBorderColor window bc = do
    let vp = toValueParam [(CWBorderPixel, fi bc)]
    connection $-> \c -> io $ changeWindowAttributes c window vp


setBorderWidth :: (MonadReader Setup m, MonadIO m) => WindowId -> Word -> m ()
setBorderWidth window bw = do
    let vp = toValueParam [(ConfigWindowBorderWidth, fi bw)]
    connection $-> \c -> io $ configureWindow c window vp


changeAttributes :: (MonadReader Setup m, MonadIO m, BitEnum e)
                 => WindowId -> [(e, Word32)] -> m ()
changeAttributes window valueparams = connection $-> \c ->
    io $ changeWindowAttributes c window $ toValueParam valueparams


focus :: (MonadReader Setup m, MonadIO m) => WindowId -> m ()
focus window = do
    config . focusedBorderColor $-> setBorderColor window -- client
    let mk_setinputfocus = MkSetInputFocus InputFocusNone
                                           window
                                           (toValue TimeCurrentTime)
    connection $-> io . flip setInputFocus mk_setinputfocus


unfocus :: (MonadReader Setup m, Functor m, MonadIO m) => WindowId -> m ()
unfocus window = do
    config . normalBorderColor $-> setBorderColor window
    connection $-> io . getInputFocus >>= void . io . getReply


raise :: (MonadReader Setup m, MonadIO m) => WindowId -> m ()
raise = flip configure [(ConfigWindowStackMode, toValue StackModeAbove)]


lower :: (MonadReader Setup m, MonadIO m) => WindowId -> m ()
lower = flip configure [(ConfigWindowStackMode, toValue StackModeBelow)]


configure :: (MonadReader Setup m, MonadIO m)
          => WindowId -> [(ConfigWindow, Word32)] -> m ()
configure w vs = connection $-> \c -> io $ configureWindow c w $ toValueParam vs


attributes :: (MonadReader Setup m, MonadIO m, Functor m)
           => WindowId -> m (Receipt GetWindowAttributesReply)
attributes window = connection $-> io . flip getWindowAttributes window


geometry :: (MonadReader Setup m, MonadIO m, Functor m)
         => WindowId -> m (Receipt GetGeometryReply)
geometry window = connection $-> io . flip getGeometry (convertXid window)
