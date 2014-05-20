-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Component where

import Data.Typeable
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Catch (bracket)

import Log
import Util
import Lens.Family.Stock
import Types


getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


-- execComponents :: (MonadWriter [String] m, Dispatcher a) => Setup -> a -> [Component] -> ReaderT Setup m [Component]
-- execComponents :: (MonadReader r m, MonadIO m, Functor m, Dispatcher a)
               -- => a -> [Component] -> ReaderT Setup m [Component]
execComponents :: (Consumer a)
               => [Component] -> a -> ReaderT Setup IO [Component]
execComponents = flip (mapM . eventDispatcher)


-- bar :: ReaderT Setup (WriterT [String] m) ()
-- bar = undefined

-- foo :: Monad m => ReaderT Setup m ()
-- foo = do
--     runStack $ ask >>= runReaderT bar
--     return ()


-- execStack :: Setup -> m a -> m a
-- execStack :: Monad m => SetupRT (LogWT m) () -> SetupRT m [String]
-- execStack :: (MonadReader Setup m) => ReaderT Setup (WriterT [String] m) a -> m [String]
-- execStack f = execWriterT $ ask >>= runReaderT f

-- execStack :: (MonadReader Setup m) => ReaderT Setup (WriterT [String] m) a -> m [String]
-- execStack :: MonadReader Setup m => SetupRT (LogWT m) a -> m [String]
-- execStack f = ask >>= execWriterT . runReaderT f
-- execStack :: (MonadReader a m, MonadIO m) => SetupRT (LogWT IO) b -> m b

-- runStack :: (MonadReader r m, MonadIO m) => ReaderT r (WriterT b IO) c -> m (c, b)
-- runStack f = ask >>= io . runWriterT . runReaderT f

runStack :: ReaderT a (WriterT w m) b -> a -> m (b, w)
runStack f = runWriterT . runReaderT f


-- eventDispatcher :: (MonadReader r m, MonadIO m, Functor m, Dispatcher a)
eventDispatcher :: (MonadIO m, Functor m, Consumer a)
                => a -> Component -> SetupRT m Component
eventDispatcher event (Component cdata runc su sd hs) = do
    run >>= _1 (io . printLog . snd) >>= returnComponent . snd
    where
    run = ask >>= io . flip runc cdata . runStack (mapM (consume event) hs)
    returnComponent d = return $ Component d runc su sd hs



withComponents :: ([Component] -> ReaderT Setup IO ())
               -> [Component]
               -> ReaderT Setup IO ()
withComponents f cs = bracket startup shutdown f
    where
    startup = mapM startupComponent cs
    shutdown = mapM_ shutdownComponent


startupComponent :: Component -> ReaderT Setup IO Component
startupComponent (Component cdata runc startupc sd hs) =
    ask >>= io . runStack (startupc cdata)
            >>= _2 (io . printLog)
                >>= returnComponent . fst
    where returnComponent d = return $ Component d runc startupc sd hs


shutdownComponent :: Component -> ReaderT Setup IO ()
shutdownComponent (Component cdata _ _ shutdownc _) =
    ask >>= io . runStack (shutdownc cdata) >>= io . printLog . snd
