-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Component where

import Data.Typeable
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception (bracket)
import Control.Concurrent
import Control.Concurrent.STM

import Log
import Util
import Lens
import Types


getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


runStack :: ReaderT a (WriterT w m) b -> a -> m (b, w)
runStack f = runWriterT . runReaderT f


execComponent :: Sink a => Setup -> a -> Component -> IO Component
execComponent setup event (Component cdata runc su sd hs) =
    run >>= _1 (printLog . snd) >>= returnComponent . snd
    where
    run = flip runc cdata $ runStack (mapM (dispatch event) hs) setup
    returnComponent d = return $ Component d runc su sd hs


withComponents :: Setup -> ([TMVar Component] -> IO a) -> IO a
withComponents setup = bracket startup shutdown
    where
    startup = do
        cvars <- mapM newTMVarIO $ setup ^. config . components
        mapM (forkIO . startupComponent setup) cvars
            >>= putStrLn . ("startup done: " ++) . show
        return cvars

    shutdown = mapM_ (shutdownComponent setup)


startupComponent :: Setup -> TMVar Component -> IO ()
startupComponent setup = flip modifyTMVarM exec
    where
    exec (Component cdata runc startupc sd hs) = do
        -- putStrLn $ "startupComponent"
        runStack (startupc cdata) setup
            >>= _2 printLog
                >>= return . \(d, _) -> Component d runc startupc sd hs
        -- (\(d, _) -> Component d runc startupc sd hs)
        -- <$> (runStack (startupc cdata) setup
        --     >>= _2 printLog)
                -- >>= returnComponent . fst

    -- returnComponent d = return $ Component d runc startupc sd hs


shutdownComponent :: Setup -> TMVar Component -> IO ()
shutdownComponent setup = flip withTMVarM exec
    where
    exec (Component cdata _ _ shutdownc _) =
        runStack (shutdownc cdata) setup >>= printLog . snd
