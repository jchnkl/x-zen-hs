-- vim: set sw=4 sws=4 ts=4


module Controller where

import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB hiding (Setup) -- (Event, SomeEvent, fromEvent, waitForEvent)

import Log
import Lens
import Util
import Types
import Component


xEventSource :: SetupRT IO AnyEvent
xEventSource = askL connection >>= fmap AnyEvent . io . waitForEvent


dispatchAnyEvent :: MonadIO m => AnyEvent -> Component -> Z m Component
dispatchAnyEvent (AnyEvent e) (Component cid d runio su sd handlers) = do
    model <- get
    ((l, model'), d') <- exec model d [] $ handlers d
    put model'
    when (not $ null l) $ appendLog $ [cid ++ ":"] ++ map ("\t"++) l
    return $ Component cid d' runio su sd handlers

    where
    exec model cdata l []     = return ((l, model), cdata)
    exec model cdata l (h:hs) = do
        ((l', model'), cdata') <- run model cdata h
        exec model' cdata' (l ++ l') hs

    run model cdata (SomeHandler h) = do
        setup <- ask
        io $ runio (execStack setup model (dispatch h e)) cdata


runController :: [SetupRT IO AnyEvent] -> SetupRT IO [(ThreadId, TChan AnyEvent)]
runController = mapM run
    where
    run f = ask >>= \setup -> io $ do
        chan <- newTChanIO
        tid <- forkIO . forever $ runReaderT f setup >>= atomically . writeTChan chan
        return (tid, chan)


runComponentsOnce :: MonadIO m => [TChan AnyEvent] -> [Component] -> Z m [Component]
runComponentsOnce chans = (readAnyEvent >>=) . run
    where
    run cs ae = mapM (dispatchAnyEvent ae) cs
    readAnyEvent = io . atomically . foldr1 orElse . map readTChan $ chans
