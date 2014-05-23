-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Component where

import Data.Typeable
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Catch (bracket)

import Log
import Lens
import Lens.Family.Stock
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


withComponents :: Setup -> ([Component] -> IO a) -> IO a
withComponents setup = bracket startup shutdown
    where
    startup = mapM (startupComponent setup) (setup ^. config . components)
    shutdown = mapM_ (shutdownComponent setup)


startupComponent :: Setup -> Component -> IO Component
startupComponent setup (Component cdata runc startupc sd hs) =
    runStack (startupc cdata) setup
        >>= _2 printLog
            >>= returnComponent . fst
    where returnComponent d = return $ Component d runc startupc sd hs


shutdownComponent :: Setup -> Component -> IO ()
shutdownComponent setup (Component cdata _ _ shutdownc _) =
    runStack (shutdownc cdata) setup >>= printLog . snd
