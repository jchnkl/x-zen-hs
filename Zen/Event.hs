{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs, RankNTypes #-}

module Event where

import Data.Maybe (catMaybes)
import Data.List ((\\))
import qualified Data.Map as M
import Data.Word
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Graphics.XHB
import Graphics.X11.Types (xK_Num_Lock, xK_Caps_Lock)

import Log
import LensUtil
import Util
import Types -- hiding (focus)
-- import Core
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


-- append default handlers
dispatch :: SomeEvent -> Z ()
dispatch e = mapM_ try <*$ eventHandler
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
handleConfigureRequest e = do
    -- connection $-> \c -> io $ configureWindow c window values
    -- configureGeometry window (Just $ Position x' y') (Just $ Dimension w' h')
    -- toLog "ConfigureRequestEvent"
    -- io (pollForError c) >>= handleError
    configure window values
    where
    -- x'     = fi $ x_ConfigureRequestEvent e
    -- y'     = fi $ y_ConfigureRequestEvent e
    -- w'     = fi $ width_ConfigureRequestEvent e
    -- h'     = fi $ height_ConfigureRequestEvent e
    window = window_ConfigureRequestEvent e
    values = copyValues e (value_mask_ConfigureRequestEvent e)
    -- values = toValueParam $ copyValues e (value_mask_ConfigureRequestEvent e)


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
-- handleDestroyNotify = unmanage . window_DestroyNotifyEvent
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
    -- unfocus (event_FocusOutEvent e)
    -- where isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e

-- runHandler' :: Maybe (KeyButtonHandler e) -> e -> Z ()
-- runHandler' Nothing _ = return ()
-- runHandler' (Just (KeyButtonHandler _ rf)) e = rf e

-- handleButtonPress :: ButtonPressEvent -> Z ()
-- handleButtonPress e = do
--     toLog "ButtonPressEvent"
--     modmask <- askL (modMask <.> config)
--     mask <- (\\ modmask) <$> cleanMask (state_ButtonPressEvent e)
--     let lookup = M.lookup (mask, fromValue $ detail_ButtonPressEvent e)
--     flip handlePress e =<< asksL (buttonHandler <.> config) lookup
--     -- return ()
--     -- when (null $ modmask \\ cleanmask) $ buttonHandler <.> config $->
--         -- runHandler . M.lookup (cleanmask \\ modmask, button)


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

    -- forM_ keysyms $ 
    --     (keyHandler <.> config $->) .       flip handlePress e . M.lookup . (,) mask
        -- flip handlePress e . M.lookup (mask, keysym) <-$ keyHandler <.> config
    
    -- keyHandler <.> config $->
    --     runHandler . M.lookup (cleanmask \\ modmask, fi keysym)



{-
handleKeyPress :: KeyPressEvent -> Z ()
handleKeyPress e = do
    toLog "KeyPressEvent"

    modmask <- askL (modMask <.> config)
    cleanmask <- cleanMask $ state_KeyPressEvent e
    return ()

    -- when (null $ modmask \\ cleanmask) $ do
    --     keysyms <- keycodeToKeysym key <$> askL keyboardMap
    --     forM_ keysyms $ \keysym -> keyHandler <.> config $->
    --         runHandler . M.lookup (cleanmask \\ modmask, fi keysym)

    where
    key = detail_KeyPressEvent e
    -- runHandler Nothing = return ()
    -- runHandler (Just (KeyEventHandler pf _)) = pf e



handleKeyRelease :: KeyReleaseEvent -> Z ()
handleKeyRelease e = do
    toLog "KeyReleaseEvent"
    toLog $ show e

    modmask <- askL (modMask <.> config)
    cleanmask <- cleanMask $ state_KeyReleaseEvent e
    return ()

    -- when (null $ modmask \\ cleanmask) $ do
    --     keysyms <- keycodeToKeysym key <$> askL keyboardMap
    --     forM_ keysyms $ \keysym -> keyHandler <.> config $->
    --         runHandler . M.lookup (cleanmask \\ modmask, fi keysym)

    where
    key = detail_KeyReleaseEvent e
    -- runHandler Nothing = return ()
    -- runHandler (Just (KeyEventHandler _ rf)) = rf e
-}


-- Closure!
-- moveWindow :: Position -> MotionNotifyEvent -> Z ()
-- moveWindow :: MotionNotifyEvent -> Z ()
moveWindow e = toLog "moveWindow" >> tryMove . (fmap (view pointer) . M.lookup window) <*$ queue
    where
    window = event_MotionNotifyEvent e
    root_x = fi $ root_x_MotionNotifyEvent e
    root_y = fi $ root_y_MotionNotifyEvent e
    values (Position x' y') = [(ConfigWindowX, root_x - fi x'),
                               (ConfigWindowY, root_y - fi y')]
    tryMove p = void $ whenJust p $ configure window . values

    -- configure c w (Position x' y') = io $ configureWindow c w $
    --                              toValueParam [(ConfigWindowX, root_x - fi x'),
    --                                            (ConfigWindowY, root_y - fi y')]


-- Closure!
-- resizeWindow :: Position -> Dimension -> MotionNotifyEvent -> Z ()
resizeWindow :: MotionNotifyEvent -> Z ()
resizeWindow e = do
    toLog "resizeWindow"
    -- TODO: do this with focused client
    void $ connection $-> withClient' window . configure


    where
    window = event_MotionNotifyEvent e
    newx = fi (root_x_MotionNotifyEvent e)
    newy = fi (root_y_MotionNotifyEvent e)

    -- TODO: use lenses
    configure c (Client _ (Geometry _ (Dimension w' h')) (Position oldx oldy)) = do
        -- Position oldx oldy <- getsL pointer
        let neww = fi w' + fi (newx - oldx)
            newh = fi h' + fi (newy - oldy)
            values = toValueParam [(ConfigWindowWidth, neww),
                                   (ConfigWindowHeight, newh)]
        toLog $ "resize to " ++ show neww ++ "x" ++ show newh
        io $ configureWindow c window values


{-


handleCreateNotify :: CreateNotifyEvent -> Z ()
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    insertWindow window
    grabButtons window
    where window = window_CreateNotifyEvent e


handleDestroyNotify :: DestroyNotifyEvent -> Z ()
handleDestroyNotify e = do
    toLog "DestroyNotifyEvent"
    removeWindow (window_DestroyNotifyEvent e) >> return True


handleEnterNotify :: EnterNotifyEvent -> Z ()
handleEnterNotify e = do
    toLog "EnterNotifyEvent"
    case detail_EnterNotifyEvent e of
        NotifyDetailInferior -> return False
        _ -> withClient (event_EnterNotifyEvent e) focus >> return True


handleLeaveNotify :: LeaveNotifyEvent -> Z ()
handleLeaveNotify e = do
    toLog "LeaveNotifyEvent"
    case detail_LeaveNotifyEvent e of
        NotifyDetailInferior -> return False
        _ -> withClient (event_LeaveNotifyEvent e) unfocus >> return True



handleMapRequest :: MapRequestEvent -> Z ()
handleMapRequest e = do
    toLog "MapRequestEvent"
    io $ print e
    connection $-> io . flip mapWindow event_window
    where
    event_window = window_MapRequestEvent e


handleConfigureRequest :: ConfigureRequestEvent -> Z ()
handleConfigureRequest e = do
    toLog "ConfigureRequestEvent"
    c <- askL connection
    io $ do
        print e
        configureWindow c win values
    io (pollForError c) >>= handleError
    where
    win = window_ConfigureRequestEvent e
    values = toValueParam $ copyValues (value_mask_ConfigureRequestEvent e)

    copyValues :: [ConfigWindow] -> [(ConfigWindow, Word32)]
    copyValues (ConfigWindowX           : ms) =
        (ConfigWindowX,           fi $ x_ConfigureRequestEvent e)               : copyValues ms
    copyValues (ConfigWindowY           : ms) =
        (ConfigWindowY,           fi $ y_ConfigureRequestEvent e)               : copyValues ms
    copyValues (ConfigWindowWidth       : ms) =
        (ConfigWindowWidth,       fi $ width_ConfigureRequestEvent e)           : copyValues ms
    copyValues (ConfigWindowHeight      : ms) =
        (ConfigWindowHeight,      fi $ height_ConfigureRequestEvent e)          : copyValues ms
    copyValues (ConfigWindowBorderWidth : ms) =
        (ConfigWindowBorderWidth, fi $ border_width_ConfigureRequestEvent e)    : copyValues ms
    copyValues (ConfigWindowSibling     : ms) =
        (ConfigWindowSibling,     convertXid $ sibling_ConfigureRequestEvent e) : copyValues ms
    copyValues (ConfigWindowStackMode   : ms) =
        (ConfigWindowStackMode,   toValue $ stack_mode_ConfigureRequestEvent e) : copyValues ms
    copyValues _ = []
-
-}


{-





defaultKeyPressHandler :: KeyPressHandler
defaultKeyPressHandler = M.empty
    -- [ (xK_Tab, \e@(KeyPressEvent _ _ _ _ _ _ _ _ _ s _)
    --                 | KeyButMaskShift `elem` s = toLog "Focus prev!"
    --                 | otherwise                = toLog "Focus next!"
    --     )
    --     ]
        -- forM_ (map fi [xK_Tab]) $ \keysym -> do
        --     whenJust (keysymToKeycode keysym kbdmap) $ \keycode ->
        --         io $ grabKey c $ MkGrabKey True (getRoot c) [ModMask1] keycode
        --                                        GrabModeAsync GrabModeAsync


defaultKeyReleaseHandler :: KeyReleaseHandler
defaultKeyReleaseHandler = M.empty


defaultButtonPressHandler :: ButtonPressHandler
defaultButtonPressHandler = M.fromList
    [ (ButtonIndex1, \e -> do
            toLog "Press ButtonIndex1"
            let window = event_ButtonPressEvent e
                event_x = event_x_ButtonPressEvent e
                event_y = event_y_ButtonPressEvent e

            raise window
            pointer ^:= Position (fi event_x) (fi event_y)
            pushHandler $ EventHandler moveWindow
      )

    , (ButtonIndex3, \e -> do
            toLog "Press ButtonIndex2"
            let window = event_ButtonPressEvent e
            lower window
      )

    , (ButtonIndex2, \e -> do
            toLog "Press ButtonIndex3"
            let window = event_ButtonPressEvent e
                root_x = root_x_ButtonPressEvent e
                root_y = root_y_ButtonPressEvent e
                w' = fi . width_GetGeometryReply
                h' = fi . height_GetGeometryReply
                update g = modifyClient window $ modL (dimension <.> geometry) $ const $ Dimension (w' g) (h' g)

            raise window
            -- TODO: do this with event_{x,y} and save pointer position in client
            pointer ^:= Position (fi root_x) (fi root_y)
            void $ flip whenRight update =<< io . getReply =<<
                withConnection (io . flip getGeometry (convertXid window))
            pushHandler $ EventHandler resizeWindow
      )
    ]


defaultButtonReleaseHandler :: ButtonReleaseHandler
defaultButtonReleaseHandler = M.fromList
    [ (ButtonIndex1, const $ do
        toLog "Release ButtonIndex1"
        popHandler
      )
    , (ButtonIndex3, const $ do
        toLog "Release ButtonIndex2")
    , (ButtonIndex2, const $ do
        toLog "Release ButtonIndex3"
        popHandler
      )
    ]

    -}
