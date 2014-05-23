{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad.Reader
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Graphics.XHB (Connection, SomeEvent, CW(..), EventMask(..))
import qualified Graphics.XHB as X

import Util
import Lens
import Lens.Family.Stock
import Types
import Config (defaultConfig)

import Message
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
main = X.connect >>= flip startup defaultConfig


eventSource :: Setup -> IO SomeEvent
eventSource setup = X.waitForEvent (setup ^. connection)


startup :: Maybe Connection -> Config -> IO ()
startup Nothing _ = print "Got no connection!"
startup (Just c) conf = do
    let mask = CWEventMask
        values = X.toMask [ EventMaskSubstructureRedirect
                          , EventMaskSubstructureNotify
                          , EventMaskFocusChange
                          ]
        valueparam = X.toValueParam [(mask, values)]
    X.changeWindowAttributes c (X.getRoot c) valueparam

    -- TODO: ungrab / regrab keys for MappingNotifyEvent

    withSetup c conf (ap withComponents run)

    where

    run :: Setup -> [Component] -> IO ()
    run setup cs = do
        tlock <- newTMVarIO []
        cvars <- mapM newTMVarIO cs

        void $ runSomeSource tlock setup cvars (eventSource setup)
        -- tids <- runSomeSource setup cvars (messageSource setup)

        waitForChildren tlock



    -- children :: Either SomeError QueryTreeReply -> [WindowId]
    -- children (Left _) = []
    -- children (Right reply) = children_QueryTreeReply reply


-- runReaderT (f :: ReaderT Setup IO ()) setup
withSetup :: -- MonadReader r m
          -- => Connection
             Connection
          -> Config
          -> (Setup -> IO a)
          -> IO a
withSetup c conf f = do
    let min_keycode = X.min_keycode_Setup $ X.connectionSetup c
        max_keycode = X.max_keycode_Setup (X.connectionSetup c) - min_keycode + 1
    kbdmap <- keyboardMapping c =<< X.getKeyboardMapping c min_keycode max_keycode
    modmap <- modifierMapping =<< X.getModifierMapping c
    f $ Setup conf c (X.getRoot c) kbdmap modmap


runSomeSource :: Sink a => ThreadLock -> Setup -> [TMVar Component] -> IO a -> IO [ThreadId]
runSomeSource tlock setup cvars f =
    uncurry (:) <$> (run <$> newBroadcastTChanIO >>= _1 id >>= _2 id)
    where run = runSource tlock f &&& runSinks tlock setup cvars


runSource :: ThreadLock -> IO a -> TChan a -> IO ThreadId
runSource tlock f chan =
    fork tlock . forever $ f >>= atomically . writeTChan chan


runSinks :: Sink a => ThreadLock -> Setup -> [TMVar Component] -> TChan a -> IO [ThreadId]
runSinks tlock setup cvars chan =
    mapM (fork tlock . (dupchan >>=) . runSink setup) cvars
    where dupchan = atomically $ dupTChan chan


runSink :: Sink a => Setup -> TMVar Component -> TChan a -> IO ()
runSink setup cvar = forever . (exec =<<) . atomically . readTChan
    where exec = modifyTMVarM cvar . execComponent setup


type ThreadLock = TMVar [TMVar ()]


fork :: ThreadLock -> IO () -> IO ThreadId
fork tlock f = do
    lock <- newEmptyTMVarIO
    modifyTMVar tlock (lock :)
    forkFinally f (const $ atomically $ putTMVar lock ())


waitForChildren :: ThreadLock -> IO ()
waitForChildren tlock =
    unlessM (atomically $ checkLocks tlock) $ waitForChildren tlock


checkLocks :: ThreadLock -> STM Bool
checkLocks tlock = takeTMVar tlock >>= \case
    []   -> return False
    l:ls -> putTMVar tlock ls >> takeTMVar l >> return True
