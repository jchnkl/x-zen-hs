-- vim:sw=4:sts=4:ts=4

module AnyEvent where

import Control.Monad (forever)
import Control.Monad.Reader (ask, runReaderT)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, writeTChan)

import Util
import Types


dispatchAnyEvent :: Monad m => AnyEvent -> Component m -> m (Component m)
dispatchAnyEvent (AnyEvent e) (Component cid d execc su sd handlers) = do
    d' <- execc (mapM_ dispatchEvent $ handlers d) d
    return $ Component cid d' execc su sd handlers
    where dispatchEvent (SomeHandler h) = dispatch h e


runEventSources :: [SetupRT IO AnyEvent] -> SetupRT IO [(ThreadId, TChan AnyEvent)]
runEventSources = mapM run
    where
    run f = ask >>= \setup -> io $ do
        chan <- newTChanIO
        tid <- forkIO . forever $ runReaderT f setup >>= atomically . writeTChan chan
        return (tid, chan)
