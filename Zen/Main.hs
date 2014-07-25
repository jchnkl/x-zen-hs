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
import System.IO (hPutStrLn, stderr)
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
import AnyEvent
import View
import Controller
import XcbEventSource

import Keyboard
import Component


initialModel :: Model
initialModel = Model (ClientQueue [] Nothing [])


-- TODO
-- getXcbSetup :: MonadReader XcbSetup m => m XcbSetup
-- avoid SetupRT for Controllers

eventSources :: [EventSource]
eventSources = [xcbEventSource]


mainLoop :: [TChan AnyEvent] -> [ControllerComponent] -> [ViewComponent] -> MainStack ()
mainLoop chans ccs vcs = do
    ((ccs', ccl), configs) <- runControllers chans ccs
    (vcs', vcl)            <- runViews vcs configs
    logPrinter $-> io . ($ ccl ++ vcl)
    mainLoop chans ccs' vcs'


runMainLoop :: [(ThreadId, TChan AnyEvent)] -> SetupRT IO ()
runMainLoop tcs = evalStateT run initialModel
                  `finally` mapM_ (io . killThread) threads
    where
    threads  = map fst tcs
    channels = map snd tcs
    run = do
        withControllerComponents $ \ccs -> do
            withViewComponents $ \vcs -> do
                mainLoop channels ccs vcs


-- TODO: move with*Component to withSetup or withConfig
--       provide components through reader
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
        , _logPrinter  = printer (Fifo "/tmp/zen.errlog")
        , _keyboardMap = kbdmap
        , _modifierMap = modmap
        }


startup :: Config -> Maybe Connection -> IO ()
startup _ Nothing     = hPutStrLn stderr "Could not connect to X server."
startup conf (Just c) = do
    let mask = CWEventMask
        values = X.toMask [ EventMaskSubstructureRedirect
                          , EventMaskSubstructureNotify
                          , EventMaskFocusChange
                          ]
        valueparam = X.toValueParam [(mask, values)]
    X.changeWindowAttributes c (X.getRoot c) valueparam

    -- TODO: ungrab / regrab keys for MappingNotifyEvent

    withSetup c conf $ runEventSources eventSources >>= runMainLoop


main :: IO ()
main = X.connect >>= startup defaultConfig
