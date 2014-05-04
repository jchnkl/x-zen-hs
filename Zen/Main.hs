{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Data.Time (getZonedTime)
import Graphics.XHB hiding (Setup)
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor

import Util
import Lens
-- import Core
import Types hiding (config)
import Config (defaultConfig)
-- import Setup hiding (config)
import Window
import Keyboard

import SomeState

import Event
import Core
import Button

-- TODO:
-- Free Monads for Layout
-- Split in proper modules
--   Pointer -> grabButton, *Cursor, etc.
--   Keyboard -> grabKeys, etc.
--   or generic X module?

-- IDEAS
-- Use Mod4 with lock after timeout
-- data BorderColor = BorderColor { _normal :: Word | _focused :: Word | etc.


-- | Cursors to be loaded at program startup
-- Cursors are available through @_glyphCursors@ in @Setup@
cursorGlyphs :: [Glyph]
cursorGlyphs =
    [ xC_fleur
    , xC_top_side
    , xC_top_right_corner
    , xC_top_left_corner
    , xC_bottom_side
    , xC_bottom_right_corner
    , xC_bottom_left_corner
    , xC_right_side
    , xC_left_side
    , xC_sizing
    ]

eventMaskButton :: [EventMask]
eventMaskButton =
    [ EventMaskButtonMotion
    , EventMaskButtonPress
    , EventMaskButtonRelease
    ]


someStates :: [SomeState]
someStates = [baseEventHandler, coreState, pointerState]


main :: IO ()
main = connect >>= startup

startup :: Maybe Connection -> IO ()
startup Nothing = print "Got no connection!"
startup (Just c) = do
    let mask = CWEventMask
        values = toMask [EventMaskSubstructureRedirect, EventMaskSubstructureNotify, EventMaskFocusChange]
        valueparam = toValueParam [(mask, values)]
    changeWindowAttributes c (getRoot c) valueparam

    withSetup c $ \setup -> do
        -- TODO: ungrab / regrab keys for MappingNotifyEvent
        -- grabKeys c config setup

        initSomeState setup someStates >>= run setup

    where
    run setup states = waitForEvent c >>= runSomeState setup states >>= run setup

    runZ :: Z ()
    runZ = connection $-> io . waitForEvent >>= dispatch


    children :: Either SomeError QueryTreeReply -> [WindowId]
    children (Left _) = []
    children (Right reply) = children_QueryTreeReply reply


withSetup :: Connection -> (Setup -> IO a) -> IO a
withSetup c f = do
    f $ Setup defaultConfig c (getRoot c) eventMaskButton M.empty M.empty M.empty



grabKeys :: Connection -> Config -> Setup -> IO ()
grabKeys c conf setup = do
    let modmask = conf ^. modMask
        kbdmap = setup ^. keyboardMap
        modmap = setup ^. modifierMap
        keys = M.keys (conf ^. keyHandler)
        nl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Num_Lock) kbdmap modmap]
        cl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Caps_Lock) kbdmap modmap]
        -- TODO: separate function
        combos m kc = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]
        grab (mask, keycode) = grabKey c $ MkGrabKey True (getRoot c)
                                                     mask keycode
                                                     GrabModeAsync GrabModeAsync

    ungrabKey c $ MkUngrabKey (toValue GrabAny) (getRoot c) [ModMaskAny]

    forM_ keys $ \(mask, keysym) ->
        whenJust (keysymToKeycode (fi keysym) kbdmap) $
            mapM_ grab . combos (modmask ++ mask)
