-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ImpredicativeTypes, FlexibleInstances, NoMonomorphismRestriction, DeriveDataTypeable, ConstraintKinds, TypeFamilies, ExistentialQuantification, GADTs, RankNTypes, StandaloneDeriving, ScopedTypeVariables #-}

module Controller where

import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB hiding (Setup) -- (Event, SomeEvent, fromEvent, waitForEvent)

import Log
import Lens
import Util
import Types hiding (Sink, dispatch)
import Component


data SomeEventHandler = forall a. (Event a) =>
    SomeEventHandler (forall m. MonadIO m => a -> Z' m ())
    deriving (Typeable)

instance Handler SomeEventHandler

class Dispatcher a where
    dispatch :: MonadIO m => a -> SomeHandler -> Z' m ()

instance Dispatcher SomeEvent where
    dispatch se sh = case fromHandler sh :: Maybe SomeEventHandler of
        Just (SomeEventHandler f) -> case fromEvent se of
            Just e -> f e
            _ -> return ()
        _ -> return ()

data AnyEvent = forall a. Dispatcher a => AnyEvent a

type EventSource = Setup -> IO AnyEvent


xEventSource :: Setup -> IO AnyEvent
xEventSource setup = AnyEvent <$> waitForEvent (setup ^. connection)


execAnyEvent :: Setup -> Model -> AnyEvent -> Component -> IO ((Log, Model), Component)
execAnyEvent setup m (AnyEvent e) (Component d runpure runio su sd somehandlers) = do
    ((runlog, model'), d') <- exec [] m d $ somehandlers d
    return $ ((runlog, model'), Component d' runpure runio su sd somehandlers)
    where
    exec runlog model cdata []     = return ((runlog, model), cdata)
    exec runlog model cdata (h:hs) = do
        ((runlog', model'), cdata') <- run model cdata h
        exec (runlog ++ runlog') model' cdata' hs
    run model cdata h = runio (execStack (dispatch e h) setup model) cdata


dispatchAnyEvent :: AnyEvent -> Component -> Z' IO Component
dispatchAnyEvent (AnyEvent e) (Component d runpure runio su sd handlers) = do
    (runlog, d') <- exec [] d $ handlers d
    -- toLog $ showLog runlog
    return $ Component d' runpure runio su sd handlers
    where
    exec runlog cdata []     = return (runlog, cdata)
    exec runlog cdata (h:hs) = do
        (runlog', cdata') <- run cdata h
        -- toLog runlog'
        exec (runlog ++ runlog') cdata' hs

    run cdata h = do
        s <- ask
        m <- get
        ((rlog, m'), cdata') <- io $ runio (execStack (dispatch e h) s m) cdata
        put m'
        return (rlog, cdata')


deriving instance Typeable SomeEvent


runController :: Setup -> [Setup -> IO AnyEvent] -> IO [(ThreadId, TChan AnyEvent)]
runController setup = mapM run
    where
    run f = do
        chan <- newTChanIO
        tid <- forkIO $ forever $ f setup >>= atomically . writeTChan chan
        return (tid, chan)


runConsumers :: [TChan AnyEvent] -> [Component] -> Z' IO ()
runConsumers chans components = readAnyEvent >>= run components
    where
    run cs ae = mapM (dispatchAnyEvent ae) cs >>= (readAnyEvent >>=) . run
    readAnyEvent = io . atomically . foldr1 orElse . map readTChan $ chans
