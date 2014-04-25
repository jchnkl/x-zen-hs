{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

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
import Types hiding (focus)
-- import Core
import Window

-- handler :: [EventHandler (Z ())]
-- handler =
--     [ EventHandler handleMapRequest
--     , EventHandler handleConfigureRequest
--     -- , EventHandler handleCreateNotify
--     -- , EventHandler handleDestroyNotify
--     -- , EventHandler handleEnterNotify
--     -- , EventHandler handleLeaveNotify
--     -- , EventHandler handleButtonPress
--     -- , EventHandler handleButtonRelease
--     -- , EventHandler handleKeyPress
--     -- , EventHandler handleKeyRelease
--     ]


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


dispatch :: SomeEvent -> Z ()
-- dispatch e = eventHandler $*> mapM_ (\(EventHandler h) -> run' (fromEvent e) h)
-- dispatch e = eventHandler $*> mapM_ (\(EventHandler h) -> run' (fromEvent e) h)
-- dispatch e = eventHandler $*> mapM_ (run' (fromEvent e))
dispatch e = mapM_ try <*$ eventHandler
    where
    try :: EventHandler (Z ()) -> Z ()
    try (EventHandler handler) = void $ whenJust (fromEvent e) handler

    -- try :: [EventHandler (Z ())] -> Z ()
    -- try hs = forM_ hs $ \(EventHandler h) -> run h (fromEvent e)
    -- try (EventHandler h:hs) = run h (fromEvent e) >> try hs
    -- try _ = return ()

    -- run :: (e -> Z ()) -> Maybe e -> Z ()
    -- run _ Nothing = return ()
    -- run handle (Just event) = handle event

    -- foo :: Event e => e -> EventHandler (Z ()) -> Z ()
    -- foo event (EventHandler handler) = handler event

    -- run (Just event) handler = handler event

-- handler :: [EventHandler (Z ())]
-- handler =
--     [ EventHandler handleMapRequest
--     , EventHandler handleConfigureRequest
--     -- , EventHandler handleCreateNotify
--     -- , EventHandler handleDestroyNotify
--     -- , EventHandler handleEnterNotify
--     -- , EventHandler handleLeaveNotify
--     -- , EventHandler handleButtonPress
--     -- , EventHandler handleButtonRelease
--     -- , EventHandler handleKeyPress
--     -- , EventHandler handleKeyRelease
--     ]

    -- [ EventHandler $ \e -> do
    --     let event_window = window_MapRequestEvent e
    --     toLog "MapRequestEvent"
    --     io $ print e
    --     connection $-> io . flip mapWindow event_window
    --     return True

    -- , EventHandler $ \e -> do
    --     let win = window_ConfigureRequestEvent e
    --         values = toValueParam $ copyValues e (value_mask_ConfigureRequestEvent e)
    --     toLog "ConfigureRequestEvent"
    --     c <- asksL connection
    --     io $ do
    --         print e
    --         configureWindow c win values
    --     io (pollForError c) >>= handleError
    --     return True


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


handleCreateNotify :: CreateNotifyEvent -> Z ()
handleCreateNotify = manage .  window_CreateNotifyEvent


handleDestroyNotify :: DestroyNotifyEvent -> Z ()
handleDestroyNotify = unmanage . window_DestroyNotifyEvent
    -- toLog "DestroyNotifyEvent"
    -- removeWindow (window_DestroyNotifyEvent e) >> return True


handleEnterNotify :: EnterNotifyEvent -> Z ()
handleEnterNotify e = when (not isInferior) $ focus (event_EnterNotifyEvent e)
    -- toLog "EnterNotifyEvent"
    where isInferior = NotifyDetailInferior == detail_EnterNotifyEvent e


handleLeaveNotify :: LeaveNotifyEvent -> Z ()
handleLeaveNotify e = when (not isInferior) $ focus (event_LeaveNotifyEvent e)
    -- toLog "LeaveNotifyEvent"
    where isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e
    -- case detail_LeaveNotifyEvent e of
    --     NotifyDetailInferior -> return False
    --     _ -> withClient (event_LeaveNotifyEvent e) unfocus >> return True


cleanMask :: [KeyButMask] -> Z [ModMask]
cleanMask mask = do
    kbdmap <- asksL $ keyboardMap
    modmap <- asksL $ modifierMap
    let keycodes = catMaybes [ keysymToKeycode (fi xK_Num_Lock) kbdmap
                             , keysymToKeycode (fi xK_Caps_Lock) kbdmap
                             ]
        modifier = catMaybes $ map (flip keycodeToModifier modmap) $ keycodes
    return $ map (fromBit . toBit) mask \\ map (fromBit . toValue) modifier


handleButtonPress :: ButtonPressEvent -> Z ()
handleButtonPress e = do
    toLog "ButtonPressEvent"
    modmask <- asksL (modMask <.> config)
    cleanmask <- cleanMask $ state_ButtonPressEvent e
    when (null $ modmask \\ cleanmask) $ buttonHandler <.> config $->
        runHandler . M.lookup (cleanmask \\ modmask, button)

    where
    button = fromValue $ detail_ButtonPressEvent e
    runHandler Nothing = return ()
    runHandler (Just (ButtonEventHandler pf _)) = pf e


handleButtonRelease :: ButtonReleaseEvent -> Z ()
handleButtonRelease e = do
    toLog "ButtonReleaseEvent"
    modmask <- asksL (modMask <.> config)
    cleanmask <- cleanMask $ state_ButtonReleaseEvent e
    when (null $ modmask \\ cleanmask) $ buttonHandler <.> config $->
        runHandler . M.lookup (cleanmask \\ modmask, button)
        -- asksL (buttonHandler <.> config) >>= runHandler . M.lookup (mask, button)

    where
    button = fromValue $ detail_ButtonReleaseEvent e
    runHandler Nothing = return ()
    runHandler (Just (ButtonEventHandler _ rf)) = rf e


handleKeyPress :: KeyPressEvent -> Z ()
handleKeyPress e = do
    toLog "KeyPressEvent"

    modmask <- asksL (modMask <.> config)
    cleanmask <- cleanMask $ state_KeyPressEvent e
    when (null $ modmask \\ cleanmask) $ do
        keysyms <- keycodeToKeysym key <$> asksL keyboardMap
        forM_ keysyms $ \keysym -> keyHandler <.> config $->
            runHandler . M.lookup (cleanmask \\ modmask, fi keysym)

    where
    key = detail_KeyPressEvent e
    runHandler Nothing = return ()
    runHandler (Just (KeyEventHandler pf _)) = pf e



handleKeyRelease :: KeyReleaseEvent -> Z ()
handleKeyRelease e = do
    toLog "KeyReleaseEvent"
    toLog $ show e

    modmask <- asksL (modMask <.> config)
    cleanmask <- cleanMask $ state_KeyReleaseEvent e
    when (null $ modmask \\ cleanmask) $ do
        keysyms <- keycodeToKeysym key <$> asksL keyboardMap
        forM_ keysyms $ \keysym -> keyHandler <.> config $->
            runHandler . M.lookup (cleanmask \\ modmask, fi keysym)

    where
    key = detail_KeyReleaseEvent e
    runHandler Nothing = return ()
    runHandler (Just (KeyEventHandler _ rf)) = rf e


handleFocusIn :: FocusInEvent -> Z ()
handleFocusIn = toLog . show

handleFocusOut :: FocusOutEvent -> Z ()
handleFocusOut = toLog . show

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
    c <- asksL connection
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



moveWindow :: MotionNotifyEvent -> Z ()
moveWindow e = do
    withConnection $ \c -> getsL pointer >>= configure c window
    where
    root_x = fi $ root_x_MotionNotifyEvent e
    root_y = fi $ root_y_MotionNotifyEvent e
    window = event_MotionNotifyEvent e
    configure c w (Position x' y') = io $ configureWindow c w $
                                 toValueParam [(ConfigWindowX, root_x - fi x'),
                                               (ConfigWindowY, root_y - fi y')]


resizeWindow :: MotionNotifyEvent -> Z ()
resizeWindow e = do
    toLog "resizeWindow"
    -- TODO: do this with focused client
    void $ withConnection $ withClient window . configure


    where
    window = event_MotionNotifyEvent e
    newx = fi (root_x_MotionNotifyEvent e)
    newy = fi (root_y_MotionNotifyEvent e)

    configure c (Client _ (Geometry _ (Dimension w' h'))) = do
        Position oldx oldy <- getsL pointer
        let neww = fi w' + fi (newx - oldx)
            newh = fi h' + fi (newy - oldy)
            values = toValueParam [(ConfigWindowWidth, neww),
                                   (ConfigWindowHeight, newh)]
        toLog $ "resize to " ++ show neww ++ "x" ++ show newh
        io $ configureWindow c window values


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
