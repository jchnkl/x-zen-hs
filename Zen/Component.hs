-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Component where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
-- import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Arrow
-- import Control.Applicative ((<$>))
import Control.Exception (bracket)
-- import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB (SomeEvent, fromEvent)

import Log
import Lens
import Lens.Family.Stock
import Util
import Types hiding (runStack, dispatch)


-- data Component = forall r m d. (Monad m, Functor m, Typeable d, Show d) => Component
--     { -- | Component data
--       componentData :: d
--       -- | Evaluation function
--     , runComponent :: forall a. m a -> d -> IO (a, d)
--     -- | Function to run on startup
--     , onStartup :: Component -> Z IO Component
--     -- | Function to run on shutdown
--     , onShutdown :: Component -> Z IO ()
--     -- -- | Update Component after every dispatched event
--     -- , postHandler :: Maybe ([r] -> Component -> Z IO Component)
--     -- | Event handler
--     , eventHandler :: Component -> [SomeHandler (Z m r)]
--     }





-- class Typeable d => ComponentClass d where
--     runComponent_ :: forall a m. MonadIO m => m a -> d -> IO (a, d)
--     handler_ :: forall e m. d -> [e -> Z m ()]

-- instance ComponentClass Int where
--     runComponent_ = runStateT

-- data SomeComponent = forall d. ComponentClass d => SomeComponent d

-- dispatch :: Setup -> e -> SomeComponent -> IO SomeComponent
-- dispatch setup e sc@(SomeComponent d) = exec sc (handler_ d)
--     where exec sc' []       = return sc'
--           exec sc' (h : hs) = execComponent setup (h e) sc' >>= flip exec hs

-- execComponent :: forall m a. Setup -> Z m a -> SomeComponent -> IO SomeComponent
-- execComponent setup f (SomeComponent d) = do
--     ((_, logs), d') <- runComponent_ (runStack setup f) d
--     printLog logs
--     return $ SomeComponent d'


-- execComponent :: Setup -> SomeData -> Component -> IO Component
-- execComponent setup event (Component cdata runc s c) = do
--     -- putStrLn $ "execComponent: " ++ show cdata
--     -- cdata' <- exec cdata handler
--     let cdata' = cdata
--     return (Component cdata runc s c)

    -- where
    -- exec d []       = return d
    -- exec d (f : hs) = do
    --     -- putStrLn $ "exec handler!"
    --     (((_, logs), hscmds), d') <- runc (runStack setup $ try f) d
    --     -- (_, ((_, logs), d') <- runc (runStack setup $ try f) d
    --     printLog logs
    --     exec d' hs

    -- try (SomeHandler conv f) = whenJustM_ (fromData event >>= conv) f


-- runComponents :: Setup -> IO ()
-- runComponents setup = withComponents setup cs . loop =<< dupChans
--     where
--     cs = (setup ^. config . components)
--     dupChans = atomically $ sequence $ map dupTChan $ setup ^. messageQueue
--     loop chans cs' = atomically (foldr1 orElse $ map readTChan chans)
--                      >>= forM cs' . execComponent setup >>= loop chans




-- instance Producer SomeEvent where
--     runProducer tchan = io . atomically . writeTChan tchan
--                             =<< io . waitForEvent <-$ connection


-- instance Message Int

getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


-- eventDispatcher' :: (Functor m, Monad m)
--                 => [SomeHandler (m ())]
--                 -> SomeEvent
--                 -> m ()
-- eventDispatcher' hs = forM_ hs . try
--     where
--     try :: (Functor m, Monad m) => SomeEvent -> SomeHandler (m ()) -> m ()
--     try event (SomeHandler h) = whenJustM_ (fromEvent event) h


eventDispatcher :: (Functor m, Monad m)
                => [EventHandler (m ())]
                -> SomeEvent
                -> m ()
eventDispatcher hs = forM_ hs . try
    where
    try :: (Functor m, Monad m) => SomeEvent -> EventHandler (m ()) -> m ()
    try event (EventHandler h) = whenJustM_ (fromEvent event) h


-- runStack :: r -> WriterT w (ReaderT r m) a -> m (a, w)
-- runStack :: r -> WriterT w1 (WriterT w (ReaderT r m)) a -> m ((a, w1), w)
runStack :: r -> WriterT w (ReaderT r m) a -> m (a, w)
runStack setup f = runReaderT (runWriterT f) setup


withComponents :: Setup -> [Component] -> ([Component] -> IO ()) -> IO ()
withComponents = undefined
{-
withComponent :: Setup -> Component -> (Component -> IO a) -> IO a
withComponent setup c = bracket startup' cleanup'
    where startup' = startupComponent setup c
          cleanup' = cleanupComponent setup


withComponents :: Setup -> [Component] -> ([Component] -> IO ()) -> IO ()
withComponents setup cslst f = exec [] cslst
    where
    exec cs' [] = f cs'
    exec cs' (c : cs) = bracket (startup' c) cleanup' (flip exec cs . (: cs'))

    startup' c = startupComponent setup c
    cleanup' = cleanupComponent setup

-}

startupComponent :: Setup -> Component -> IO Component
startupComponent setup (Component cdata runc startupc c h) =
    runStack setup (startupc cdata) >>= _2 printLog  >>= returnC . fst
    where returnC d = return $ Component d runc startupc c h

{-


cleanupComponent :: Setup -> Component -> IO Component
cleanupComponent setup (Component cdata runc s cleanupc) = do
    -- (_, logs) <- runStack setup (cleanupc cdata)
    ((_, logs), _) <- runStack setup $ cleanupc cdata
    printLog logs
    return (Component cdata runc s cleanupc)
-}

-- updateHandler :: [HandlerManageOps]
--                -> Map Int (SomeHandler ) -> Map Int (SomeHandler )
-- updateHandler (h:hs) hm = foldr exec hm hs
--     where exec (AttachHandler k v) = M.insert k v
--           exec (DetachHandler k)   = M.delete k


-- startComponents :: Setup -> [Component] -> IO [ThreadId]
-- startComponents setup = undefined -- mapM (startComponent setup)


{-
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
-}


-- runConsumer :: [Consumer] -> Z IO [ThreadId]
-- runConsumer tids (Consumer c:cs) = do
--     tchan <- newBroadcastTChanIO
--     writer <- forkIO $ someProducer writeSome tchan
--     reader <- atomically (dupTChan tchan) >>= forkIO . someConsumer c
--
--     produce (writer : reader : tids) cs
--
-- produce tids _ = return tids


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
-- class ComponentDispatcher a where
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


-- someEventHandler :: [TMVar Component] -> Z IO ()
-- someEventHandler cvars = do
--     connection $-> io . waitForEvent >>= forM_ cvars . (io . withTMVar . dispatch)


-- dispatch :: a -> Component -> Z IO Component
-- dispatch event (Component cdata runc s c hs) = do
--     -- cdata' <- run runc event cdata hs
--     return (Component cdata runc s c hs)

    -- where
    -- run :: Setup -> Z m () -> Component -> IO Component
    -- run setup f (Component cdata runc s c hs) = do
    --     ((_, logs), cdata') <- runc (runStack setup f) cdata
    --     return (Component cdata' runc s c hs)

    -- run _ e d []       = return d
    -- run r e d (f : fs) = do
    --     ((_, logs), d') <- r (runStack setup (f e)) d
    --     printLog logs
    --     run r e d' fs

-- runComponents :: Setup -> IO ()
-- runComponents setup = do
--     cvars <- mapM newTMVarIO cs
--
--     -- forM_ (_eventSources setup) (forkIO . ($ cvars))
--     forM_ ([someEventHandler setup]) (forkIO . ($ cvars))
--
--     where
--     cs = setup ^. config .components






-- attachHandler :: Monad m => SomeHandler -> Z m ()
-- attachHandler e = lift $ tell [e]

-- runComponent_ :: Setup -> Component -> IO Component
-- runComponent_ setup = run
--     -- runReaderT (runWriterT f) setup
--     where
--     run (Component cdata runc s c) = do
--         -- ((_, logs), cdata') <- runc (runStack setup f) cdata
--         -- ((_, logs), cdata') <- runc (runStack setup f) cdata
--         let cdata' = cdatA
--         return (Component cdata' runc s c)
