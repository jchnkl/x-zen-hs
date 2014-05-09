{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}

module Component where

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception (bracket)
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


runStack :: r -> WriterT w1 (WriterT w (ReaderT r m)) a -> m ((a, w1), w)
runStack setup f = runReaderT (runWriterT (runWriterT f)) setup


withComponents :: Setup -> [Component] -> ([Component] -> IO a) -> IO a
withComponents setup cs = bracket startup' terminate'
    where startup' = initializeComponents setup cs
          terminate' = terminateComponents setup


initializeComponents :: Setup -> [Component] -> IO [Component]
initializeComponents setup = startup' []
    where
    startup' result (Component c runc startupc t e m : cs) = do
        ((c', logs), _) <- runStack setup (startupc c)
        printLog logs
        startup' (Component c' runc startupc t e m : result) cs
    startup' result _ = return result


terminateComponents :: Setup -> [Component] -> IO [Component]
terminateComponents setup = cleanup' []
    where
    cleanup' result (Component c runc s cleanupc e m : cs) = do
        ((_, logs), _) <- runStack setup (cleanupc c)
        printLog logs
        cleanup' (Component c runc s cleanupc e m : result) cs
    cleanup' result _ = return result


runComponents :: Setup -> SomeEvent -> [Component] -> IO [Component]
runComponents setup event = run' []
    where
    run' result (Component c runc i t hevent hmsg : scs) = do
        (((_, logs), _), c') <- runc (runStack setup $ hevent event) c
        printLog logs
        run' (Component c' runc i t hevent hmsg : result) scs
    run' result _ = return result


handleMessages :: Setup -> [SomeMessage] -> Component -> IO Component
handleMessages setup (msg:msgs) (Component c runc i t hevent hmsg) = do
    (((_, logs), msgs'), c') <- runc (runStack setup $ hmsg msg) c
    printLog logs
    handleMessages setup (msgs ++ msgs') (Component c' runc i t hevent hmsg)
handleMessages _ _ sc = return sc


