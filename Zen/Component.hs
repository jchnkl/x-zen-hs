-- vim:sw=4:sts=4:ts=4

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
import Xproto


withComponents :: ([Component] -> ModelST (SetupRT IO) a) -> ModelST (SetupRT IO) a
withComponents = bracket startup shutdown
    where
    startup = askL (config . components) >>= mapM startupComponent
    shutdown = mapM_ shutdownComponent


startupComponent :: Component -> ModelST (SetupRT IO) Component
startupComponent (Component cid d r startup su h) = do
    (d', l) <- connection $-> flip runXprotoT (runWriterT (startup d))
    io . printLog $ ("startup " ++ cid ++ ":") : l
    return $ Component cid d' r startup su h


shutdownComponent :: Component -> ModelST (SetupRT IO) ()
shutdownComponent (Component cid d _ _ shutdown _) = do
    l <- connection $-> flip runXprotoT (execWriterT (shutdown d))
    io . printLog $ ("shutdown " ++ cid ++ ":") : l


runStack :: MonadIO m => Setup -> Model -> Z m a -> m ((a, Log), Model)
runStack setup m f = runReaderT (runStateT (runXprotoT c $ runWriterT f) m) setup
    where c = setup ^. connection


execStack :: MonadIO m => Setup -> Model -> Z m () -> m (Log, Model)
execStack setup m f = runReaderT (runStateT (runXprotoT c $ execWriterT f) m) setup
    where c = setup ^. connection
