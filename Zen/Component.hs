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
         -> WriterT a (WriterT w (ReaderT r (StateT s m))) a1
         -> s
         -> m ((a, w), s)
runStack setup f = runStateT (runReaderT (runWriterT (execWriterT f)) setup)

initComponent :: Setup -> [Component] -> IO [Component]
initComponent setup states = run [] states
    where
    run result [] = return result

    run result (Stateful i s f : somestates) = do
        ((logs, msgs), s') <- runStack setup i s
        printLog logs
        run (Stateful i s f : result) somestates

    run result (s : somestates) = run (s : result) somestates


runComponent :: Setup -> [Component] -> SomeEvent -> IO [Component]
runComponent setup states event = run [] states
    where
    run result [] = return result

    run result (Stateful i s f : somestates) = do
        ((logs, msgs), s') <- runStack setup (f event) s
        printLog logs
        run (Stateful i s f : result) somestates

    run result (Stateless f : somestates) = do
        (logs, msgs) <- runReaderT (runWriterT (execWriterT (f event))) setup
        printLog logs
        run (Stateless f : result) somestates
