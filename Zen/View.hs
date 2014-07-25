-- vim:sw=4:sts=4:ts=4

module View where

import Control.Monad (forM)
import Control.Monad.Catch (bracket)
import Control.Monad.State (get)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (runWriterT, execWriterT)

import Log
import Lens
import Util
import Types
import Component


runViewStack :: ViewStack a -> MainStack (a, Log)
runViewStack = lift . runWriterT


execViewStack :: ViewStack a -> MainStack Log
execViewStack = lift . execWriterT


runViews :: [ViewComponent] -> ClientConfigs -> MainStack ([ViewComponent], Log)
runViews vcs configs = runWriterT (get >>= forM vcs . run >>= mapM (run configs))
    where
    run t c@(Component cid d execc su sd handlers) = do
        (d', l) <- lift . lift . runWriterT
                               $ execc (mapM_ (doDispatch t) (handlers d)) d
        appendComponentLog c l
        return $ Component cid d' execc su sd handlers
    doDispatch t (SomeHandler h) = dispatch h t


startupViewComponents :: [ViewComponent] -> MainStack [ViewComponent]
startupViewComponents = startup []
    where
    startup cs' (c@(Component{componentId = cid}):cs) = do
        (c', l) <- runViewStack $ startupComponent c
        logPrinter $-> io . ($ ("startup " ++ cid ++ ":") : (map ("\t"++) l))
        startup (c':cs') cs
    startup cs' _ = return $ reverse cs'


shutdownViewComponents :: [ViewComponent] -> MainStack ()
shutdownViewComponents (c@(Component{componentId = cid}):cs) = do
    l <- execViewStack $ shutdownComponent c
    logPrinter $-> io . ($ ("shutdown " ++ cid ++ ":") : (map ("\t"++) l))
    shutdownViewComponents cs
shutdownViewComponents _ = return ()


withViewComponents :: ([ViewComponent] -> MainStack a) -> MainStack a
withViewComponents f = (config . viewComponents) $-> \cs -> bracket (startup cs) shutdown f
    where startup = startupViewComponents
          shutdown = shutdownViewComponents
