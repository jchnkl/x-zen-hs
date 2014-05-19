{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

-- import qualified Data.Map as M
-- import qualified Data.Set as S
import Control.Monad
-- import Control.Monad.State
-- import Control.Monad.Reader
-- import Control.Monad.Writer
import Control.Applicative
import Control.Exception hiding (mask)
import Control.Concurrent
import Control.Concurrent.STM
-- import Data.Time (getZonedTime)
import Graphics.XHB hiding (Setup)

import Util
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

    -- startThreads :: Setup -> IO [ThreadId]
    -- startThreads = flip startComponents (conf ^. components)


        -- write = atomically . writeTChan (setup ^. eventQueue) . toMessage

    -- children :: Either SomeError QueryTreeReply -> [WindowId]
    -- children (Left _) = []
    -- children (Right reply) = children_QueryTreeReply reply


-- eventLoop :: Connection -> TChan SomeMessage -> IO ()
-- eventLoop c chan = forever $ waitForEvent c
--                              >>= atomically . writeTChan chan . toMessage
-- -- eventLoop c chan = forever $ waitForEvent c >>= \e -> whenJustM_ (fromEvent e)
-- --                              (atomically . writeTChan chan . toMessage)


withSetup :: Connection
          -> Config
          -- -> [TChan SomeMessage -> IO ()]
          -> (Setup -> IO a)
          -> IO a
withSetup c conf f = do
    let min_keycode = min_keycode_Setup $ connectionSetup c
        max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
    kbdmap <- keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode
    modmap <- modifierMapping =<< getModifierMapping c

    f $ Setup conf c (getRoot c) kbdmap modmap

    -- f (Setup conf c (getRoot c) kbdmap modmap chans)
    --     `finally` (mapM killThread tids)


    -- -- putStrLn $ "sources: " ++ show (length sources)
    -- chans <- replicateM (length sources) newBroadcastTChanIO
    -- -- putStrLn $ "chans: " ++ show (length chans)
    -- tids <- mapM (uncurry runThread) (zip sources chans)

    -- f (Setup conf c (getRoot c) kbdmap modmap chans)
    --     `finally` (mapM killThread tids)

    -- where
    -- runThread :: (TChan SomeMessage -> IO ()) -> TChan SomeMessage -> IO ThreadId
    -- runThread = (forkIO .)
