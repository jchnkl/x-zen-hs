{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Window where

import Data.Word
import Control.Monad
import Control.Monad.IO.Class
import Graphics.XHB

import Lens
import Util
import Types


setBorderColor :: MonadIO m => WindowId -> Word -> Z m ()
setBorderColor window bc = do
    let vp = toValueParam [(CWBorderPixel, fi bc)]
    connection $-> \c -> io $ changeWindowAttributes c window vp


setBorderWidth :: MonadIO m => WindowId -> Word -> Z m ()
setBorderWidth window bw = do
    let vp = toValueParam [(ConfigWindowBorderWidth, fi bw)]
    connection $-> \c -> io $ configureWindow c window vp


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
