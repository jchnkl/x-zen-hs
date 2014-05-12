{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Component where

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB (SomeEvent, fromEvent, waitForEvent)

import Log
import Lens
import Util
import Types

deriving instance Typeable SomeEvent

instance Producer SomeEvent where
    runProducer tchan = io . atomically . writeTChan tchan
                            =<< io . waitForEvent <-$ connection


instance Message Int

instance Producer SomeMessage where
    runProducer tchan = do
        io $ threadDelay (1 * 1000 * 1000)
        io $ atomically (writeTChan tchan $ SomeMessage (1 :: Int))


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
startComponents setup = undefined -- mapM (startComponent setup)


-- startComponent :: Setup -> Component -> IO ThreadId
startComponent :: Component -> Z IO ()
startComponent component = do
    

-- mapM (atomically . dupTChan) queues >>=
--     forkIO . void . withComponent setup component . runLoop

    c <- newTMVarIO component

    -- forM_ channels $ \channel -> do
    --     mapM (flip withChannel channel) fooxfun
    --     -- foo channel
    --     -- case (foo channel undefined) of
    --     --     Nothing -> return ()
    --     --     Just chan -> atomically (readTChan chan)
    --     return ()

    --     atomically (readChannel channel) >>= flip whenJustM (dispatchOnComponent setup c)
        -- readChannel setup (fromChannel channel) c

    --     whenJustM (fromChannel channel) $ \chan -> (readQ chan >>= withElement c)
        -- fmap (withElement c) (readQ (fromChannel channel) >>=) 
        -- atomically (readQ $ fromChannel channel) >>= withElement c

    return ()


runConsumer :: [Consumer] -> Z IO [ThreadId]
runConsumer tids (Consumer c:cs) = do
    tchan <- newBroadcastTChanIO
    writer <- forkIO $ someProducer writeSome tchan
    reader <- atomically (dupTChan tchan) >>= forkIO . someConsumer c

    produce (writer : reader : tids) cs

produce tids _ = return tids


    -- listen :: TChan a -> TMVar Component -> IO ()
    -- channels :: [Channel]
    -- channels = [setup ^. eventQueue, setup ^. messageQueue]

    -- queues :: [TChan SomeElement]
    -- queues = [setup ^. eventQueue, setup ^. messageQueue]

    -- runLoop :: [TChan SomeElement] -> Component -> IO ()
    -- runLoop chans c =
    --     atomically (foldr1 orElse $ map readTChan chans)
    --     >>= dispatch c
    --     >>= runLoop chans

    -- runLoop2 :: [TChan SomeElement] -> TMVar Component -> IO ()
    -- runLoop2 chans cmv = do
    --     mapM forkIO (listen cmv) chans
    --     atomically (foldr1 orElse $ map readTChan chans)
    --     >>= dispatch c
    --     >>= runLoop chans

    -- withElement :: ComponentDispatcher a => TMVar Component -> Maybe a -> IO ()
    -- withElement cmv (Just e) = atomically (takeTMVar cmv)
    --                     >>= dispatchWithComponent setup e
    --                     >>= atomically . putTMVar cmv


    -- -- foo :: 
    -- foo = do
    --     componentMVar <- newTMVarIO component

--     dispatch :: Component -> SomeElement -> IO Component
--     dispatch component'@(Component c runc i t hevent hmsg) e
--         | Just sm <- fromElement e :: Maybe SomeMessage = do
--             ((_, logs), c') <- runc (runStack setup $ hmsg sm) c
--             printLog logs
--             return (Component c' runc i t hevent hmsg)
--
--         | Just se <- fromElement e :: Maybe SomeEvent = do
--             ((_, logs), c') <- runc (runStack setup $ hevent se) c
--             printLog logs
--             return (Component c' runc i t hevent hmsg)
--
--         | otherwise = putStrLn "Nothing" >> return component'

    -- dispatch c@Component { handleEvent = hevent, handleMessage = hmsg }
    --     | Nothing = return c
    --     | (Just x) = undefined -- c' runc (runStack undefined) c
    --     | (Just SomeMessage) = undefined -- c' runc (runStack undefined) c

    -- listenForEvent chan (Component c runc i t hevent hmsg) = do
    --     ((_, logs), c') <- flip runc c . runStack setup . hevent
    --                        =<< atomically (readTChan chan)
    --     printLog logs
    --     listenForEvent chan (Component c' runc i t hevent hmsg)

    -- listenForMessage chan (Component c runc i t hevent hmsg) = do
    --     ((_, logs), c') <- flip runc c . runStack setup . hmsg
    --                        =<< atomically (readTChan chan)
    --     printLog logs
    --     listenForMessage chan (Component c' runc i t hevent hmsg)


modifyComponent :: TMVar Component -> (Component -> IO Component) -> IO ()
modifyComponent c f = atomically (takeTMVar c) >>= f >>= atomically . putTMVar c


{-
-- data Handler b = forall a. (a -> b) => Handler a
data Handler b = forall a. ChannelClass a => Handler (a -> b)

fooEventHandler :: SomeEvent -> IO b
fooEventHandler = undefined

fooMessageHandler :: SomeMessage -> IO b
fooMessageHandler = undefined

fooxfun :: [Handler (IO b)]
fooxfun = [Handler fooEventHandler, Handler fooMessageHandler]

-- withChannel :: forall a b. ChannelClass a => (a -> IO b) -> Channel -> IO (Maybe b) -- Z m ()
withChannel :: Handler (IO b) -> Channel -> IO (Maybe b) -- Z m ()
withChannel (Handler h) c = atomically (readChannel c) >>= flip whenJustM h

-- withChannels :: [Channel] -> Handler (IO ()) -> IO () -- Z m ()
withChannels :: forall a b. ChannelClass a => (a -> IO b) -> [Channel] -> IO () -- Z m ()
withChannels handler (c:cs) = do
    -- atomically (readChannel c) >>= try
    return ()

    where
    try :: Channel -> IO ()
    try chan = whenJustM_ (fromChannel chan) (try' handler)

    -- try' :: forall a. ChannelClass a => TChan a -> IO ()
    try' :: forall a b. (a -> IO b) -> TChan a -> IO b
    try' handler chan = atomically (readTChan chan) >>= handler
-}

-- readChannel :: Setup -> Maybe (TChan SomeEvent) -> TMVar Component -> IO ()
-- readChannel = undefined

-- class ChannelClass a => ComponentDispatcher a where
class ComponentDispatcher a where
    -- readQ :: Maybe (TChan a) -> Maybe (IO a)
    -- readQ = fmap (atomically . readTChan)

    -- readChannel :: Setup -> Maybe (TChan a) -> TMVar Component -> IO ()
    -- readChannel _ Nothing _ = return ()
    -- readChannel setup (Just chan) c = atomically (readTChan chan)
    --                                   >>= modifyComponent c . dispatchItem setup

    -- readQ :: Maybe (TChan a) -> STM (Maybe a)
    -- readQ Nothing = return Nothing
    -- readQ (Just chan) = Just $ readTChan chan

    -- foo :: a -> IO ()
    -- foo = undefined

    -- foo (Channel c) = return ()
    -- foo channel _ = (fromChannel channel :: Maybe a)

    -- dispatchOnComponent :: Setup -> TMVar Component -> a -> IO ()
    -- dispatchItem :: Setup -> a -> Component -> IO Component
    -- dispatchWithComponent :: Setup -> a -> Component -> IO Component

    -- bar :: a -> IO ()
    -- bar = undefined


-- instance ComponentDispatcher SomeEvent where
--     dispatchWithComponent setup event (Component c runc i t hevent hmsg) = do
--         ((_, logs), c') <- runc (runStack setup $ hevent event) c
--         printLog logs
--         return (Component c' runc i t hevent hmsg)

-- instance ComponentDispatcher SomeMessage where
--     dispatchWithComponent setup msg (Component c runc i t hevent hmsg) = do
--         ((_, logs), c') <- runc (runStack setup $ hmsg msg) c
--         printLog logs
--         return (Component c' runc i t hevent hmsg)

-- sendMessage :: Typeable s => SomeMessage -> Z (StateT s IO) ()
-- sendMessage _ = do
--     _ <- askL (config . components)
--     -- forM_ cs $ send msg
--     -- res <- send [] cs
--     -- putL (config . components) res
--     return ()
--     -- where


-- sendMessage :: MonadIO m => SomeMessage -> Z m ()
-- sendMessage msg = do
--     setup <- ask
--     cs <- askL (config . components)
--     forM_ cs $ \(Component c runc i t e hmsg) -> do
--         c' <- io $ runc (runStack setup $ hmsg msg) c
--         return ()
