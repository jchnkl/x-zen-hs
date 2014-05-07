{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}

module Component where

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Graphics.XHB (SomeEvent, fromEvent)

import Log
import Util
import Types


getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


eventDispatcher :: (Functor m, Monad m) => [EventHandler (m ())] -> SomeEvent -> m ()
eventDispatcher handler = forM_ handler . try
    where
    try :: (Functor m, Monad m) => SomeEvent -> EventHandler (m ()) -> m ()
    try event (EventHandler h) = void $ whenJust (fromEvent event) h


runStack :: (Monad m, Monoid w)
         => r
         -> WriterT a (WriterT w (ReaderT r m)) a1
         -> m (a, w)
runStack setup f = runReaderT (runWriterT (execWriterT f)) setup


runComponents :: Setup -> SomeEvent -> [Component] -> IO [Component]
runComponents setup event = run' []
    where
    run' result (Component c runc i t hevent hmsg : scs) = do
        ((logs, _), c') <- runc ((runStack setup) (hevent event)) c
        printLog logs
        run' (Component c' runc i t hevent hmsg : result) scs
    run' result _ = return result


handleMessages :: Setup -> [SomeMessage] -> Component -> IO Component
handleMessages setup (msg:msgs) (Component c runc i t hevent hmsg) = do
    ((logs, msgs'), c') <- runc ((runStack setup) (hmsg msg)) c
    printLog logs
    handleMessages setup (msgs ++ msgs') (Component c' runc i t hevent hmsg)
handleMessages _ _ sc = return sc


