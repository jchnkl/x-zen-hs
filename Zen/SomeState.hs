{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}

module SomeState where

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


runSomeState :: Setup -> [SomeState] -> SomeEvent -> IO [SomeState]
runSomeState setup states event = run [] states
    where
    run result [] = return result
    run result (Stateful s f : somestates) = do
        (logstr, s') <- runStateT (runReaderT (execWriterT (f event)) setup) s
        printLog logstr
        run (Stateful s' f : result) somestates
    run result (Stateless f : somestates) = do
        runReaderT (execWriterT (f event)) setup >>= printLog
        run (Stateless f : result) somestates
