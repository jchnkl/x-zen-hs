-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving #-}

module Component where

import Data.Typeable
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Exception (bracket)

import Log
import Lens.Family.Stock
import Types


getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


eventDispatcher :: Dispatcher a => Setup -> a -> Component -> IO Component
eventDispatcher setup event (Component cdata runc su sd hs) =
    run >>= _1 (printLog . snd) >>= returnComponent . snd
    where run = runc (runStack setup $ mapM_ (dispatch event) hs) cdata
          returnComponent d = return $ Component d runc su sd hs


runStack :: Setup -> WriterT w (ReaderT Setup m) a -> m (a, w)
runStack setup f = runReaderT (runWriterT f) setup


withComponents :: Setup -> [Component] -> ([Component] -> IO ()) -> IO ()
withComponents setup cs f = bracket startup shutdown f
    where
    startup = mapM (startupComponent setup) cs
    shutdown = mapM_ (shutdownComponent setup)


startupComponent :: Setup -> Component -> IO Component
startupComponent setup (Component cdata runc startupc sd hs) =
    runStack setup (startupc cdata) >>= _2 printLog  >>= returnComponent . fst
    where returnComponent d = return $ Component d runc startupc sd hs


shutdownComponent :: Setup -> Component -> IO ()
shutdownComponent setup (Component cdata _ _ shutdownc _) =
    runStack setup (shutdownc cdata) >>= printLog . snd
