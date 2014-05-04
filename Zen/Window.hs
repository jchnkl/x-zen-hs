{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Window where

import Data.Word
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad
import Control.Monad.IO.Class
import Graphics.XHB
import Graphics.X11.Types (xK_Num_Lock, xK_Caps_Lock)

import Lens
import Util
import Types
import Keyboard



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


grabButtons :: MonadIO m => WindowId -> Z m ()
grabButtons window = connection $-> \c -> do
    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    eventmask <- askL buttonMask
    let nl = catMaybes [fmap (fromBit . toValue) $ keysymToModifier kbdmap modmap (fi xK_Num_Lock)]
        cl = catMaybes [fmap (fromBit . toValue) $ keysymToModifier kbdmap modmap (fi xK_Caps_Lock)]
        -- TODO: separate function
        combos m b = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [b, b ..]

    buttons <- asksL (config . buttonHandler) (M.keys)

    forM_ buttons $ \(m, b) -> mapM_ (grab c eventmask) $ combos (modmask ++ m) b

    where
    grab c eventmask (mask, button) = do
        io $ grabButton c $ MkGrabButton True window eventmask
                            GrabModeAsync GrabModeAsync
                            (convertXid xidNone) (convertXid xidNone)
                            button mask

configure :: MonadIO m => WindowId -> [(ConfigWindow, Word32)] -> Z m ()
configure w vs = connection $-> \c -> io $ configureWindow c w $ toValueParam vs
