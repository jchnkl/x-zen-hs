{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad.Catch (finally)
import Graphics.XHB (Connection, SomeEvent, CW(..), EventMask(..))
import qualified Graphics.XHB as X

import Log
import Util
import Lens
import Types
import Config (defaultConfig)

import Core
import Event
import Types
import Controller

import Keyboard
import Component


{-
* TODO:
    - Free Monads for Layout
    - Split in proper modules
      - Pointer -> grabButton, *Cursor, etc.
      - Keyboard -> grabKeys, etc.
        or generic X module?

* IDEAS
 - Use Mod4 with lock after timeout
 - data BorderColor = BorderColor { _normal :: Word | _focused :: Word | etc.

 |-> Config | Normal | Manage
 |-> View:       Config determines State presentation
 |-> Controller: Config determines input interpretation
       |-> modifies State
       |-> modifies Config?

Small Core which does
 * listen for events
     |-> run through EventController, e.g. for MappingNotifyEvent, changing mode
 * run Controller with correct Mode Config -> State
 * run View with correct Mode Config & State


 * Warp mouse when focus changes
-}


initialModel :: Model
initialModel = ClientQueue [] Nothing []


controller :: [Controller]
controller = [xEventSource]


views :: [Model -> IO ()]
views = [print]


mainLoop :: [TChan AnyEvent] -> [Component] -> ModelST (SetupRT IO) ()
mainLoop chans cs = do
    (cs', l) <- runWriterT (runComponentsOnce chans cs)
    io $ printLog l
    get >>= io . forM_ Main.views . flip id
    mainLoop chans cs'


runMainLoop :: [(ThreadId, TChan AnyEvent)] -> SetupRT IO ()
runMainLoop tcs = evalStateT (withComponents . mainLoop $ map snd tcs) initialModel
                  `finally` mapM_ (io . killThread . fst) tcs


withSetup :: Connection -> Config -> (SetupRT IO a) -> IO a
withSetup c conf f = do
    let min_keycode = X.min_keycode_Setup $ X.connectionSetup c
        max_keycode = X.max_keycode_Setup (X.connectionSetup c) - min_keycode + 1
    kbdmap <- io (keyboardMapping c =<< X.getKeyboardMapping c min_keycode max_keycode)
    modmap <- io (modifierMapping =<< X.getModifierMapping c)
    runReaderT f $ Setup conf c (X.getRoot c) kbdmap modmap


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

    withSetup c conf $ runController controller >>= runMainLoop


main :: IO ()
main = X.connect >>= startup defaultConfig
