{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Event where

import Data.Maybe (catMaybes)
import Data.List ((\\))
import qualified Data.Map as M
import Data.Word
import Control.Monad
import Control.Applicative
import Graphics.XHB
import Graphics.X11.Types (xK_Num_Lock, xK_Caps_Lock)

import Log
import Lens
import Util
import Types
import Window
import Queue


class InputEventDispatcher pe re where
    dispatchPress   :: InputEventHandler pe re -> pe -> Z ()
    dispatchRelease :: InputEventHandler pe re -> re -> Z ()

    handlePress :: Maybe (InputEventHandler pe re) -> pe -> Z ()
    handlePress Nothing _ = return ()
    handlePress (Just h) e = dispatchPress h e

    handleRelease :: Maybe (InputEventHandler pe re) -> re -> Z ()
    handleRelease Nothing _ = return ()
    handleRelease (Just h) e = dispatchRelease h e

instance InputEventDispatcher KeyPressEvent KeyReleaseEvent where
    dispatchPress   h e = (press h) e
    dispatchRelease h e = (release h) e

instance InputEventDispatcher ButtonPressEvent ButtonReleaseEvent where
    dispatchPress   h e = (press h) e
    dispatchRelease h e = (release h) e


handleError :: Maybe SomeError -> Z ()
handleError Nothing = return ()
handleError (Just se) = toLog $ "ERROR: " ++ show se


popHandler :: Z ()
popHandler = eventHandler %:= safePop
    where
    safePop (_:es) = es
    safePop _ = []


pushHandler :: EventHandler (Z ()) -> Z ()
pushHandler eh = eventHandler %:= (eh :)


-- TODO: append default handlers
dispatch :: SomeEvent -> Z ()
dispatch e = mapM_ try =<< getsL eventHandler (++ defaultHandler)
    where
    try :: EventHandler (Z ()) -> Z ()
    try (EventHandler handler) = void $ whenJust (fromEvent e) handler

    defaultHandler :: [EventHandler (Z ())]
    defaultHandler =
        -- Structure control events
        [ EventHandler handleMapRequest
        , EventHandler handleConfigureRequest
        , EventHandler handleCirculateNotify
        , EventHandler handleResizeRequest

        -- Window state notification events
        , EventHandler handleCreateNotify
        , EventHandler handleDestroyNotify
        , EventHandler handleMapNotify
        , EventHandler handleUnmapNotify

        -- Window crossing events
        , EventHandler handleEnterNotify
        , EventHandler handleLeaveNotify

        -- Input focus events
        , EventHandler handleFocusIn
        , EventHandler handleFocusOut

        -- Pointer events
        , EventHandler handleButtonPress
        , EventHandler handleButtonRelease

        -- Keyboard events
        , EventHandler handleKeyPress
        , EventHandler handleKeyRelease
        ]


copyValues :: ConfigureRequestEvent -> [ConfigWindow] -> [(ConfigWindow, Word32)]
copyValues e (ConfigWindowX           : ms) =
    (ConfigWindowX,           fi $ x_ConfigureRequestEvent e)               : copyValues e ms
copyValues e (ConfigWindowY           : ms) =
    (ConfigWindowY,           fi $ y_ConfigureRequestEvent e)               : copyValues e ms
copyValues e (ConfigWindowWidth       : ms) =
    (ConfigWindowWidth,       fi $ width_ConfigureRequestEvent e)           : copyValues e ms
copyValues e (ConfigWindowHeight      : ms) =
    (ConfigWindowHeight,      fi $ height_ConfigureRequestEvent e)          : copyValues e ms
copyValues e (ConfigWindowBorderWidth : ms) =
    (ConfigWindowBorderWidth, fi $ border_width_ConfigureRequestEvent e)    : copyValues e ms
copyValues e (ConfigWindowSibling     : ms) =
    (ConfigWindowSibling,     convertXid $ sibling_ConfigureRequestEvent e) : copyValues e ms
copyValues e (ConfigWindowStackMode   : ms) =
    (ConfigWindowStackMode,   toValue $ stack_mode_ConfigureRequestEvent e) : copyValues e ms
copyValues _ _ = []


cleanMask :: [KeyButMask] -> Z [ModMask]
cleanMask mask = do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    let keycodes = catMaybes [ keysymToKeycode (fi xK_Num_Lock) kbdmap
                             , keysymToKeycode (fi xK_Caps_Lock) kbdmap
                             ]
        modifier = catMaybes $ map (flip keycodeToModifier modmap) $ keycodes
        modifiermask = map (fromBit . toBit) (mask \\ [KeyButMaskButton1 ..])
    return $ modifiermask \\ map (fromBit . toValue) modifier


-- Event handler

handleMapRequest :: MapRequestEvent -> Z ()
handleMapRequest e = do
    toLog "MapRequestEvent"
    connection $-> io . flip mapWindow (window_MapRequestEvent e)


handleConfigureRequest :: ConfigureRequestEvent -> Z ()
handleConfigureRequest e = configure window values
    where
    window = window_ConfigureRequestEvent e
    values = copyValues e (value_mask_ConfigureRequestEvent e)


handleCirculateNotify :: CirculateNotifyEvent -> Z ()
handleCirculateNotify _ = toLog "CirculateNotifyEvent"


handleResizeRequest :: ResizeRequestEvent -> Z ()
handleResizeRequest _ = toLog "ResizeRequestEvent"


handleMapNotify :: MapNotifyEvent -> Z ()
handleMapNotify _ = toLog "MapNotifyEvent"


handleUnmapNotify :: UnmapNotifyEvent -> Z ()
handleUnmapNotify _ = toLog "UnmapNotifyEvent"


handleCreateNotify :: CreateNotifyEvent -> Z ()
handleCreateNotify = manage .  window_CreateNotifyEvent


handleDestroyNotify :: DestroyNotifyEvent -> Z ()
handleDestroyNotify e = toLog "DestroyNotifyEvent" >> unmanage ( window_DestroyNotifyEvent e)


handleEnterNotify :: EnterNotifyEvent -> Z ()
handleEnterNotify e = do
    toLog "EnterNotifyEvent"
    when (not isInferior) $ focus (event_EnterNotifyEvent e)
    where isInferior = NotifyDetailInferior == detail_EnterNotifyEvent e


handleLeaveNotify :: LeaveNotifyEvent -> Z ()
handleLeaveNotify e = do
    toLog "LeaveNotifyEvent"
    when (not isInferior) $ unfocus (event_LeaveNotifyEvent e)
    where isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e


handleFocusIn :: FocusInEvent -> Z ()
handleFocusIn _ = toLog "FocusInEvent"


handleFocusOut :: FocusOutEvent -> Z ()
handleFocusOut e = do
    toLog $ "FocusOutEvent: " ++ show (mode_FocusOutEvent e)


handleButtonPress :: ButtonPressEvent -> Z ()
handleButtonPress e = do
    toLog "ButtonPressEvent"
    let state = state_ButtonPressEvent e
        button = fromValue $ detail_ButtonPressEvent e
    mask <- (\\) <$> (cleanMask state) <*> askL (config . modMask)
    flip handlePress e . M.lookup (mask, button) <-$ config . buttonHandler


handleButtonRelease :: ButtonReleaseEvent -> Z ()
handleButtonRelease e = do
    toLog "ButtonReleaseEvent"
    let state = state_ButtonReleaseEvent e
        button = fromValue $ detail_ButtonReleaseEvent e
    mask <- (\\) <$> (cleanMask state) <*> askL (config . modMask)
    flip handleRelease e . M.lookup (mask, button) <-$ config . buttonHandler


handleKeyPress :: KeyPressEvent -> Z ()
handleKeyPress e = do
    toLog "KeyPressEvent"
    let state = state_KeyPressEvent e
        keycode = detail_KeyPressEvent e

    mask <- (\\) <$> (cleanMask state) <*> askL (config . modMask)

    let lookupKeysym keysym = M.lookup (mask, fi keysym)
    mapM_ (flip handlePress e)
        =<< (mapM $ asksL (config . keyHandler) . lookupKeysym)
            =<< asksL keyboardMap (keycodeToKeysym keycode)


handleKeyRelease :: KeyReleaseEvent -> Z ()
handleKeyRelease e = do
    toLog "KeyReleaseEvent"
    let state = state_KeyReleaseEvent e
        keycode = detail_KeyReleaseEvent e

    mask <- (\\) <$> (cleanMask state) <*> askL (config . modMask)

    let lookupKeysym keysym = M.lookup (mask, fi keysym)
    mapM_ (flip handleRelease e)
        =<< (mapM $ asksL (config . keyHandler) . lookupKeysym)
            =<< asksL keyboardMap (keycodeToKeysym keycode)


moveWindow :: Position -> MotionNotifyEvent -> Z ()
moveWindow (Position x' y') e = configure window values
    where
    window = event_MotionNotifyEvent e
    root_x = fi $ root_x_MotionNotifyEvent e
    root_y = fi $ root_y_MotionNotifyEvent e
    values = [(ConfigWindowX, root_x - fi x'), (ConfigWindowY, root_y - fi y')]


resizeWindow :: Position -> Dimension -> MotionNotifyEvent -> Z ()
resizeWindow (Position x' y') (Dimension w' h') e = configure window values
    where
    window = event_MotionNotifyEvent e
    x'' = root_x_MotionNotifyEvent e
    y'' = root_y_MotionNotifyEvent e
    w'' = w' + fi x'' - fi x'
    h'' = h' + fi y'' - fi y'
    values = [(ConfigWindowWidth, fi w''), (ConfigWindowHeight, fi h'')]
