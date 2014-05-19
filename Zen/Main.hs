{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

import Graphics.XHB hiding (Setup)

import Lens
import Types
import Config (defaultConfig)

import Keyboard
import Component


{-
* TODO:
    - Free Monads for Layout
    - Split in proper modules
      - Pointer -> grabButton, *Cursor, etc.
      - Keyboard -> grabKeys, etc.
        or generic X module?

* IDEAS
 - Use Mod4 with lock after timeout
 - data BorderColor = BorderColor { _normal :: Word | _focused :: Word | etc.

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

    withSetup c conf run
        -- withComponents setup (setup ^. config . components) $ \cs ->
        -- waitForEvent c >>= execComponents setup event cs

        -- return ()
        -- tids <- startThreads setup
        -- eventLoop setup `finally` mapM_ killThread tids

    where
    run setup = withComponents setup (setup ^. config . components) (loop setup)
    loop setup cs = waitForEvent c >>= flip (execComponents setup) cs >>= loop setup

    -- children :: Either SomeError QueryTreeReply -> [WindowId]
    -- children (Left _) = []
    -- children (Right reply) = children_QueryTreeReply reply


withSetup :: Connection
          -> Config
          -> (Setup -> IO a)
          -> IO a
withSetup c conf f = do
    let min_keycode = min_keycode_Setup $ connectionSetup c
        max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
    kbdmap <- keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode
    modmap <- modifierMapping =<< getModifierMapping c

    f $ Setup conf c (getRoot c) kbdmap modmap
