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


getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


dispatchEvent :: Dispatcher a => Setup -> a -> Component -> IO Component
dispatchEvent setup event (Component cdata runc u d hs) = do
    ((_, logs), cdata') <- runc (runStack setup $ mapM_ (dispatch event) hs) cdata
    -- printLog logs
    return $ Component cdata' runc u d hs


runStack :: Setup -> WriterT w (ReaderT Setup m) a -> m (a, w)
runStack setup f = runReaderT (runWriterT f) setup


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
