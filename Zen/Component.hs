-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall #-}

module Component where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Catch (bracket)

import Log
import Lens
import Util
import Types


withComponents :: ([Component] -> ModelST (SetupRT IO) a) -> ModelST (SetupRT IO) a
withComponents = bracket startup shutdown
    where
    startup = askL (config . components) >>= mapM startupComponent
    shutdown = mapM_ shutdownComponent


startupComponent :: Component -> ModelST (SetupRT IO) Component
startupComponent (Component cid d r startup su h) = do
    (d', l) <- runWriterT (startup d)
    io . printLog $ ("startup " ++ cid ++ ":") : l
    return $ Component cid d' r startup su h


shutdownComponent :: Component -> ModelST (SetupRT IO) ()
shutdownComponent (Component cid d _ _ shutdown _) = do
    l <- execWriterT (shutdown d)
    io . printLog $ ("shutdown " ++ cid ++ ":") : l


runStack :: Monad m => Setup -> Model -> Z m a -> m ((a, Log), Model)
runStack setup model f = runReaderT (runStateT (runWriterT f) model) setup


execStack :: Monad m => Setup -> Model -> Z m () -> m (Log, Model)
execStack setup model f = runReaderT (runStateT (execWriterT f) model) setup
