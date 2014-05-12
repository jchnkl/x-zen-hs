{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- import qualified Data.Map as M
-- import qualified Data.Set as S
-- import Control.Monad.State
-- import Control.Monad.Reader
-- import Control.Monad.Writer
-- import Control.Applicative
import Control.Exception hiding (mask)
import Control.Concurrent
import Control.Concurrent.STM
-- import Data.Time (getZonedTime)
import Graphics.XHB hiding (Setup)

-- import Util
import Lens
-- import Core
import Types hiding (startup)
import Config (defaultConfig)
-- import Setup hiding (config)
-- import Window
import Keyboard

import Component

-- import Event
-- import Core
-- import Button

-- TODO:
-- Free Monads for Layout
-- Split in proper modules
--   Pointer -> grabButton, *Cursor, etc.
--   Keyboard -> grabKeys, etc.
--   or generic X module?

-- IDEAS
-- Use Mod4 with lock after timeout
-- data BorderColor = BorderColor { _normal :: Word | _focused :: Word | etc.

{-
 |-> Config | Normal | Manage
 |-> View:       Config determines State presentation
 |-> Controller: Config determines input interpretation
       |-> modifies State
       |-> modifies Config?

Small Core which does
 * listen for events
     |-> run through EventController, e.g. for MappingNotifyEvent, changing mode
 * run Controller with correct Mode Config -> State
 * run View with correct Mode Config & State
-}



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

    -- TODO: ungrab / regrab keys for MappingNotifyEvent
    -- grabKeys c config setup

    withSetup c conf $ \setup -> do
        tids <- startThreads setup
        eventLoop setup `finally` mapM_ killThread tids

    where
    startThreads :: Setup -> IO [ThreadId]
    startThreads = flip startComponents (conf ^. components)

    eventLoop :: Setup -> IO ()
    eventLoop setup = waitForEvent c >>= write >> eventLoop setup
        where write = atomically . writeTChan (setup ^. eventQueue) . toElement

    -- children :: Either SomeError QueryTreeReply -> [WindowId]
    -- children (Left _) = []
    -- children (Right reply) = children_QueryTreeReply reply


withSetup :: Connection -> Config -> (Setup -> IO a) -> IO a
withSetup c conf f = do
    let min_keycode = min_keycode_Setup $ connectionSetup c
        max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
    kbdmap <- keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode
    modmap <- modifierMapping =<< getModifierMapping c
    eventQ <- newBroadcastTChanIO
    messageQ <- newBroadcastTChanIO
    f $ Setup conf c (getRoot c) kbdmap modmap eventQ messageQ


{-
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
-}
