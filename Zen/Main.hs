{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Data.Time (getZonedTime)
import Graphics.XHB hiding (Setup)

import Util
import Lens
-- import Core
import Types hiding (startup)
import Config (defaultConfig)
-- import Setup hiding (config)
import Window
import Keyboard

import Component

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



eventMaskButton :: [EventMask]
eventMaskButton =
    [ EventMaskButtonMotion
    , EventMaskButtonPress
    , EventMaskButtonRelease
    ]


main :: IO ()
main = connect >>= flip startup defaultConfig


startup :: Maybe Connection -> Config -> IO ()
startup Nothing _ = print "Got no connection!"
startup (Just c) conf = do
    let mask = CWEventMask
        values = toMask [ EventMaskSubstructureRedirect
                        , EventMaskSubstructureNotify
                        , EventMaskFocusChange
                        ]
        valueparam = toValueParam [(mask, values)]
    changeWindowAttributes c (getRoot c) valueparam

    withSetup c conf $ ap (flip withComponents (conf ^. components)) run

        -- TODO: ungrab / regrab keys for MappingNotifyEvent
        -- grabKeys c config setup

    where
    run setup components = waitForEvent c
                            >>= flip (runComponents setup) components
                            >>= run setup

    children :: Either SomeError QueryTreeReply -> [WindowId]
    children (Left _) = []
    children (Right reply) = children_QueryTreeReply reply


withSetup :: Connection -> Config -> (Setup -> IO a) -> IO a
withSetup c conf f = do
    let min_keycode = min_keycode_Setup $ connectionSetup c
        max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
    kbdmap <- keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode
    modmap <- modifierMapping =<< getModifierMapping c
    f $ Setup conf c (getRoot c) kbdmap modmap


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
