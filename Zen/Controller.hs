-- vim:sw=4:sts=4:ts=4

module Controller where

import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB (waitForEvent)

import Data.Map as M (empty)
import Control.Monad (forever)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (runStateT)
import Control.Monad.Catch (bracket)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (runWriterT, execWriterT)

import Lens
import Util
import Types
import Model
import Component


xEventSource :: SetupRT IO AnyEvent
xEventSource = askL connection >>= fmap AnyEvent . io . waitForEvent


runControllerStack :: Z IO a -> MainStack ((a, Log), ClientConfigs)
runControllerStack f = flip runStateT M.empty $ runModelOps $ runWriterT f


execControllerStack :: Z IO a -> MainStack (Log, ClientConfigs)
execControllerStack f = flip runStateT M.empty $ runModelOps $ execWriterT f


startupControllerComponents :: [ControllerComponent] -> MainStack [ControllerComponent]
startupControllerComponents = startup []
    where
    startup cs' (c@(Component{componentId = cid}):cs) = do
        (c', l) <- fmap fst $ runControllerStack $ startupComponent c
        logPrinter $-> io . ($ ("startup " ++ cid ++ ":") : (map ("\t"++) l))
        startup (c':cs') cs
    startup cs' _ = return $ reverse cs'


shutdownControllerComponents :: [ControllerComponent] -> MainStack ()
shutdownControllerComponents (c@(Component{componentId = cid}):cs) = do
    l <- fmap fst $ execControllerStack $ shutdownComponent c
    logPrinter $-> io . ($ ("shutdown " ++ cid ++ ":") : (map ("\t"++) l))
    shutdownControllerComponents cs
shutdownControllerComponents _ = return ()


withControllerComponents :: ([ControllerComponent] -> MainStack a) -> MainStack a
withControllerComponents f =
    (config . controllerComponents) $-> \cs -> bracket (startup cs) shutdown f
    where startup = startupControllerComponents
          shutdown = shutdownControllerComponents
