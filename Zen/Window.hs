{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Window where

import Data.Word
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad
import Control.Applicative ((<$>))
import Graphics.XHB
import Graphics.X11.Types (xK_Num_Lock, xK_Caps_Lock)

import Lens
import Util
import Types
import Queue


manage :: WindowId -> Z ()
manage window = whenM (isClient <$> attributes) $ do
    configure'
    queue %:= (insert $ Client window (Geometry (Position 0 0) (Dimension 0 0)) $ Position 0 0)

    where
    attributes :: Z (Either SomeError GetWindowAttributesReply)
    attributes = io . getReply =<<
        io . flip getWindowAttributes window <-$ connection

    isClient :: Either SomeError GetWindowAttributesReply -> Bool
    isClient (Right reply) = not $ isUnviewable reply
    isClient _             = False

    isUnviewable :: GetWindowAttributesReply -> Bool
    isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r

    configure' :: Z ()
    configure' = do
        let mask = CWEventMask
            values = toMask [EventMaskEnterWindow, EventMaskLeaveWindow, EventMaskFocusChange]
            valueparam = toValueParam [(mask, values)]
        connection $-> \c -> io $ changeWindowAttributes c window valueparam
        config . borderWidth $-> setBorderWidth window
        grabButtons window


unmanage :: WindowId -> Z ()
unmanage w = queue %:= remove w


setBorderColor :: WindowId -> Word -> Z ()
setBorderColor window bc = do
    let vp = toValueParam [(CWBorderPixel, fi bc)]
    connection $-> \c -> io $ changeWindowAttributes c window vp


setBorderWidth :: WindowId -> Word -> Z ()
setBorderWidth window bw = do
    let vp = toValueParam [(ConfigWindowBorderWidth, fi bw)]
    connection $-> \c -> io $ configureWindow c window vp


focus :: WindowId -> Z ()
focus window = do
    config . focusedBorderColor $-> setBorderColor window -- client
    let mk_setinputfocus = MkSetInputFocus InputFocusNone
                                           window
                                           (toValue TimeCurrentTime)
    connection $-> io . flip setInputFocus mk_setinputfocus


unfocus :: WindowId -> Z ()
unfocus window = do
    config . normalBorderColor $-> setBorderColor window
    connection $-> io . getInputFocus >>= void . io . getReply


configure :: WindowId -> [(ConfigWindow, Word32)] -> Z ()
configure w vs = connection $-> \c -> io $ configureWindow c w $ toValueParam vs


raise :: WindowId -> Z ()
raise = flip configure [(ConfigWindowStackMode, toValue StackModeAbove)]


lower :: WindowId -> Z ()
lower = flip configure [(ConfigWindowStackMode, toValue StackModeBelow)]


grabButtons :: WindowId -> Z ()
grabButtons window = connection $-> \c -> do
    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    let nl = catMaybes [fmap (fromBit . toValue) $ keysymToModifier (fi xK_Num_Lock) kbdmap modmap]
        cl = catMaybes [fmap (fromBit . toValue) $ keysymToModifier (fi xK_Caps_Lock) kbdmap modmap]
        combos m b = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [b, b ..]

    buttons <- asksL (config . buttonHandler) (M.keys)

    forM_ buttons $ \(m, b) -> mapM_ (grab c) $ combos (modmask ++ m) b

    where
    events = [EventMaskButtonMotion, EventMaskButtonPress, EventMaskButtonRelease]
    grab c (mask, button) = do
        io $ grabButton c $ MkGrabButton True window events
                            GrabModeAsync GrabModeAsync
                            (convertXid xidNone) (convertXid xidNone)
                            button mask
