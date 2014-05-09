{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}

module Component where

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception (bracket)
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB (SomeEvent, fromEvent)

import Log
import Util
import Types


getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


eventDispatcher :: (Functor m, Monad m)
                => [EventHandler (m ())]
                -> SomeEvent
                -> m ()
eventDispatcher handler = forM_ handler . try
    where
    try :: (Functor m, Monad m) => SomeEvent -> EventHandler (m ()) -> m ()
    try event (EventHandler h) = whenJustM_ (fromEvent event) h


runStack :: r -> WriterT w (ReaderT r m) a -> m (a, w)
runStack setup f = runReaderT (runWriterT f) setup


withComponent :: Setup -> Component -> (Component -> IO a) -> IO a
withComponent setup c = bracket startup' cleanup'
    where startup' = startupComponent setup c
          cleanup' = cleanupComponent setup


startupComponent :: Setup -> Component -> IO Component
startupComponent setup (Component c runc startupc t e m) = do
    (c', logs) <- runStack setup (startupc c)
    printLog logs
    return (Component c' runc startupc t e m)


cleanupComponent :: Setup -> Component -> IO Component
cleanupComponent setup (Component c runc s cleanupc e m) = do
    (_, logs) <- runStack setup (cleanupc c)
    printLog logs
    return (Component c runc s cleanupc e m)


startComponents :: Setup -> [Component] -> IO [ThreadId]
startComponents setup = mapM (startComponent setup)


startComponent :: Setup -> Component -> IO ThreadId
startComponent setup = forkIO . flip (withComponent setup) run
    where
    run c = atomically (dupTChan $ _eventQueue setup) >>= flip loop c

    loop chan (Component c runc i t hevent hmsg) = do
        ((_, logs), c') <- flip runc c . runStack setup . hevent
                           =<< atomically (readTChan chan)
        printLog logs
        loop chan (Component c' runc i t hevent hmsg)
