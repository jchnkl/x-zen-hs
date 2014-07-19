-- vim:sw=4:sts=4:ts=4

module Controller where

import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB

import Lens
import Util
import Types


xEventSource :: SetupRT IO AnyEvent
xEventSource = askL connection >>= fmap AnyEvent . io . waitForEvent


dispatchAnyEvent :: AnyEvent -> Component s -> s (Component s)
dispatchAnyEvent (AnyEvent e) (Component cid d execc su sd handlers) = do
    d' <- execc (mapM_ (flip dispatch e) (handlers d)) d
    return $ Component cid d' execc su sd handlers


runController :: [SetupRT IO AnyEvent] -> SetupRT IO [(ThreadId, TChan AnyEvent)]
runController = mapM run
    where
    run f = ask >>= \setup -> io $ do
        chan <- newTChanIO
        tid <- forkIO . forever $ runReaderT f setup >>= atomically . writeTChan chan
        return (tid, chan)
