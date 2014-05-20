{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

import Graphics.XHB hiding (Setup)

import Util
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
    -- run setup = withComponents setup (setup ^. config . components) (loop setup)
    -- run :: ReaderT Setup IO ()
    run :: ReaderT Setup IO ()
    run = do
        cs <- askL (config . components)
        void $ withComponents loop cs

    loop :: [Component] -> ReaderT Setup IO ()
    loop cs = connection $-> io . waitForEvent >>= execComponents cs >>= loop

    -- children :: Either SomeError QueryTreeReply -> [WindowId]
    -- children (Left _) = []
    -- children (Right reply) = children_QueryTreeReply reply


-- runReaderT (f :: ReaderT Setup IO ()) setup
withSetup :: -- MonadReader r m
          -- => Connection
             Connection
          -> Config
          -- -> (Setup -> IO a)
          -> ReaderT Setup IO a
          -> IO a
withSetup c conf f = do
    let min_keycode = min_keycode_Setup $ connectionSetup c
        max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
    kbdmap <- keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode
    modmap <- modifierMapping =<< getModifierMapping c

    runReaderT f $ Setup conf c (getRoot c) kbdmap modmap


someEventSource :: SomeSource -- IO SomeEvent
someEventSource = SomeSource $ waitForEvent . (^. connection)


-- runSources :: Setup -> [Component] -> [SomeSource] -> IO ()
-- runSources _ _ [] = return ()
-- runSources setup cs (SomeSource f:srcs) = do
--     f setup >>= flip (execComponents) cs
--     runSources setup cs srcs
