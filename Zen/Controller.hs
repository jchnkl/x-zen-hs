-- vim:sw=4:sts=4:ts=4

module Controller where

import Control.Concurrent.STM (TChan, atomically, orElse, readTChan)

import Data.Map as M (empty)
import Control.Monad (forM)
import Control.Monad.State (runStateT)
import Control.Monad.Catch (bracket)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (runWriterT, execWriterT)

import Log
import Lens
import Util
import Types
import Model
import AnyEvent
import Component


runControllerStack :: Z IO a -> MainStack ((a, Log), ClientConfigs)
runControllerStack f = flip runStateT M.empty $ runModelOps $ runWriterT f


execControllerStack :: Z IO a -> MainStack (Log, ClientConfigs)
execControllerStack f = flip runStateT M.empty $ runModelOps $ execWriterT f


runControllers :: [TChan AnyEvent]
               -> [ControllerComponent]
               -> MainStack (([ControllerComponent], Log), ClientConfigs)
runControllers chans = runControllerStack . (readAnyEvent >>=) . run
    where
    run cs e = forM cs $ \c -> do
        (c', l) <- lift $ runWriterT (dispatchAnyEvent e c)
        appendComponentLog c l
        return c'
    readAnyEvent = io . atomically . foldr1 orElse . map readTChan $ chans


startupControllerComponents :: [ControllerComponent]
                            -> MainStack ([ControllerComponent], ClientConfigs)
startupControllerComponents = startup M.empty []
    where
    startup configs cs' (c@(Component{componentId = cid}):cs) = do
        ((c', l), configs') <- flip runStateT configs . runModelOps . runWriterT
                             $ startupComponent c
        logPrinter $-> io . ($ ("startup " ++ cid ++ ":") : (map ("\t"++) l))
        startup configs' ((c'):cs') cs
    startup configs cs' _ = return $ (reverse cs', configs)


shutdownControllerComponents :: [ControllerComponent] -> MainStack ()
shutdownControllerComponents (c@(Component{componentId = cid}):cs) = do
    l <- fmap fst $ execControllerStack $ shutdownComponent c
    logPrinter $-> io . ($ ("shutdown " ++ cid ++ ":") : (map ("\t"++) l))
    shutdownControllerComponents cs
shutdownControllerComponents _ = return ()


withControllerComponents :: (([ControllerComponent], ClientConfigs) -> MainStack a)
                         -> MainStack a
withControllerComponents f =
    (config . controllerComponents) $-> \cs -> bracket (startup cs) shutdown f
    where startup = startupControllerComponents
          shutdown = shutdownControllerComponents . fst
