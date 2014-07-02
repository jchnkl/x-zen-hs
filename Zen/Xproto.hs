-- vim:sw=4:sts=4:ts=4

-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}

module Xproto where

import Data.Word
import Control.Monad.Free hiding (Pure, Free)
import Control.Monad.Trans.Free
import Control.Monad.Trans (lift)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Monad.IO.Class
-- import Graphics.XHB (Connection, Receipt, SomeError, ConfigWindow(..))
import Graphics.XHB as X

import Util
import Types

-- foo :: Z IO ()
-- foo = do
--     io $ putStrLn "foo"
--     move undefined (Position 0 0)

-- bar :: IO ()
-- bar = do
--     let f = runReaderT (execStateT (execWriterT foo) undefined) undefined
--     void $ untrans f >>= runXproto undefined
--     return ()


-- runXprotoT :: MonadIO m => Connection -> FreeT XprotoF m a -> m a
-- -- runXprotoT c f = undefined -- untrans f >>= liftIO . runXproto c
-- runXprotoT c f = runFreeT f >>= runFree (runXprotoT c)


runXprotoT :: MonadIO m => Connection -> FreeT XprotoF m a -> m a
runXprotoT c m = runFreeT m >>= runXproto c


runXproto :: MonadIO m => Connection -> FreeF XprotoF a (FreeT XprotoF m a) -> m a
runXproto c = \case
    Pure a -> return a

    Free (GetReply receipt f) ->
        io (X.getReply receipt) >>= runXprotoT c . f

    Free (GetWindowAttributes win f) ->
        io (X.getWindowAttributes c win) >>= runXprotoT c . f

    Free (ConfigureWindow win vp a) -> do
        io $ X.configureWindow c win vp
        runXprotoT c a

    -- Free (PutStrLn str a) -> do
    --     io $ putStrLn str
    --     runXprotoT c a


-- liftF :: (Functor f, MonadFree f m) => f a -> m a
-- liftF = wrap . fmap return


-- liftZ :: Monad m => XprotoFT m a -> Z m a
-- liftZ = lift . lift . lift


getReplyF :: Monad m => Receipt r -> XprotoFT m (Either SomeError r)
getReplyF receipt = liftF $ GetReply receipt id

getReply :: Monad m => Receipt r -> Z m (Either SomeError r)
getReply = lift . getReplyF

getWindowAttributesF :: Monad m => WindowId -> XprotoFT m (Receipt GetWindowAttributesReply)
getWindowAttributesF win = liftF $ GetWindowAttributes win id

getWindowAttributes :: Monad m => WindowId -> Z m (Receipt GetWindowAttributesReply)
getWindowAttributes = lift . getWindowAttributesF

configureWindowF :: Monad m => WindowId -> ValueParam Word16 -> XprotoFT m ()
configureWindowF win vp = liftF $ ConfigureWindow win vp ()

configureWindow :: Monad m => WindowId -> ValueParam Word16 -> Z m ()
configureWindow w = lift . configureWindowF w

-- putStrLnF :: Monad m => String -> XprotoFT m ()
-- putStrLnF str = liftF $ PutStrLn str ()


{-
foo :: Z IO ()
foo = do
    io $ putStrLn "foo"
    move undefined (Position 0 0)

bar :: IO ()
bar = do
    let f = runReaderT (execStateT (execWriterT foo) undefined) undefined
    void $ untrans f >>= runXproto undefined
    return ()


runXprotoT :: MonadIO m => Connection -> FreeT XprotoF m a -> m a
runXprotoT c f = untrans f >>= liftIO . runXproto c


runXproto :: Connection -> Free XprotoF a -> IO a
runXproto c = \case
    (Pure a) -> return a

    (Impure (Move win (Position x' y') a)) -> do
        configureWindow c win $ toValueParam [(ConfigWindowX, fromIntegral x'),
                                              (ConfigWindowY, fromIntegral y')]
        runXproto c a

    (Impure (Resize win (Dimension w' h') a)) -> do
        io $ configureWindow c win $ toValueParam [(ConfigWindowWidth, fromIntegral w'),
                                                   (ConfigWindowHeight, fromIntegral h')]
        runXproto c a


liftF :: (Functor f, MonadFree f m) => f a -> m a
liftF = wrap . fmap return


liftZ :: Monad m => XprotoFT m a -> Z m a
liftZ = lift . lift . lift


moveF :: Monad m => WindowId -> Position -> XprotoFT m ()
moveF win pos = liftF $ Move win pos ()

move :: Monad m => WindowId -> Position -> Z m ()
move w = liftZ . moveF w

resizeF :: Monad m => WindowId -> Dimension -> XprotoFT m ()
resizeF win dim = liftF $ Resize win dim ()

resize :: Monad m => WindowId -> Dimension -> Z m ()
resize w = liftZ . resizeF w
-}
