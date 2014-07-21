-- vim:sw=4:sts=4:ts=4

{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Catch (bracket, finally)
import Graphics.XHB (Connection, SomeEvent, CW(..), EventMask(..))
import qualified Graphics.XHB as X

import Log
import Util
import Lens
import Types
import Config (defaultConfig)

import Core
import Base
import Model
import Types hiding (model)
import Controller

import Keyboard
import Component


initialModel :: Model
initialModel = Model (ClientQueue [] Nothing [])


controller :: [Controller]
controller = [xEventSource]


views :: [Model -> IO ()]
views = [print]


runStack :: Z IO a -> ModelST (SetupRT IO) ((a, Log), ClientConfigs)
runStack f = flip runStateT M.empty $ runModelOps $ runWriterT f


execStack :: Z IO a -> ModelST (SetupRT IO) (Log, ClientConfigs)
execStack f = flip runStateT M.empty $ runModelOps $ execWriterT f


mainLoop :: [TChan AnyEvent] -> [ControllerComponent] -> [ViewComponent] -> ModelST (SetupRT IO) ()
mainLoop chans ccs vcs = do
    ((ccs', ccl), configs) <- runStack (runComponents chans ccs)
    ((vcs', vcl)) <- runWriterT $ runViews vcs configs
    logPrinter $-> io . ($ ccl ++ vcl)
    mainLoop chans ccs' vcs'


runComponents :: [TChan AnyEvent]
              -> [ControllerComponent]
              -> Z IO [ControllerComponent]
runComponents chans = (readAnyEvent >>=) . run
    where
    run cs e = forM cs $ \c -> do
        (c', l) <- lift $ runWriterT (runComponent e c)
        appendComponentLog c l
        return c'
    readAnyEvent = io . atomically . foldr1 orElse . map readTChan $ chans


runMainLoop :: [(ThreadId, TChan AnyEvent)] -> SetupRT IO ()
runMainLoop tcs = do
    vcs <- askL (config . viewComponents)
    evalStateT (withComponents $ \ccs -> mainLoop (map snd tcs) ccs vcs) initialModel
    `finally` mapM_ (io . killThread . fst) tcs
    where
    withComponents f = askL (config . components) >>= flip withControllerComponents f


withControllerComponents :: [ControllerComponent]
                         -> ([ControllerComponent] -> ModelST (SetupRT IO) a)
                         -> ModelST (SetupRT IO) a
withControllerComponents cs = bracket startup shutdown
    where startup = startupControllerComponents cs
          shutdown = shutdownControllerComponents


startupControllerComponents :: [ControllerComponent]
                            -> ModelST (SetupRT IO) [ControllerComponent]
startupControllerComponents = startup []
    where
    startup cs' (c@(Component{componentId = cid}):cs) = do
        (c', l) <- fmap fst $ runStack $ startupComponent c
        logPrinter $-> io . ($ ("startup " ++ cid ++ ":") : (map ("\t"++) l))
        startup (c':cs') cs
    startup cs' _ = return $ reverse cs'


shutdownControllerComponents :: [ControllerComponent] -> ModelST (SetupRT IO) ()
shutdownControllerComponents (c@(Component{componentId = cid}):cs) = do
    l <- fmap fst $ execStack $ shutdownComponent c
    logPrinter $-> io . ($ ("shutdown " ++ cid ++ ":") : (map ("\t"++) l))
    shutdownControllerComponents cs
shutdownControllerComponents _ = return ()


withSetup :: Connection -> Config -> (SetupRT IO a) -> IO a
withSetup c conf f = do
    let min_keycode = X.min_keycode_Setup $ X.connectionSetup c
        max_keycode = X.max_keycode_Setup (X.connectionSetup c) - min_keycode + 1
    kbdmap <- io (keyboardMapping c =<< X.getKeyboardMapping c min_keycode max_keycode)
    modmap <- io (modifierMapping =<< X.getModifierMapping c)
    runReaderT f $ Setup
        { _config      = conf
        , _connection  = c
        , _rootWindow  = X.getRoot c
        , _logPrinter  = fifoPrinter "/tmp/zen.errlog"
        , _keyboardMap = kbdmap
        , _modifierMap = modmap
        }


startup :: Config -> Maybe Connection -> IO ()
startup _ Nothing     = print "Got no connection!"
startup conf (Just c) = do
    let mask = CWEventMask
        values = X.toMask [ EventMaskSubstructureRedirect
                          , EventMaskSubstructureNotify
                          , EventMaskFocusChange
                          ]
        valueparam = X.toValueParam [(mask, values)]
    X.changeWindowAttributes c (X.getRoot c) valueparam

    -- TODO: ungrab / regrab keys for MappingNotifyEvent

    -- runController :: [SetupRT IO AnyEvent] -> SetupRT IO [(ThreadId, TChan AnyEvent)]
    withSetup c conf $ runController controller >>= runMainLoop


main :: IO ()
main = X.connect >>= startup defaultConfig
