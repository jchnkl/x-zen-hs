-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable,
             ExistentialQuantification,
             MultiParamTypeClasses,
             LambdaCase,
             TupleSections #-}
             -- FlexibleContexts,
             -- RankNTypes,

module Event where

import Data.Typeable
-- import Data.Maybe (catMaybes)
-- import Data.List ((\\))
-- import qualified Data.Map as M
-- import qualified Data.Set as S
-- import qualified Data.List as L
import Data.Word
-- import Control.Monad
-- import Control.Monad.IO.Class
import Control.Applicative
import Graphics.XHB
-- import Graphics.X11.Types (xK_Num_Lock, xK_Caps_Lock)

import Log
import Lens
import Util
import Types
import Window
-- import Keyboard


instance Producer SomeEvent where
    produce setup = waitForEvent (setup ^. connection)


instance Consumer SomeEvent where
    consume event (EventHandler f) = whenJustM_ (fromEvent event) f
    consume _ _                    = return ()


{-
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
-}


{-
handleError :: Maybe SomeError -> Z ()
handleError Nothing = return ()
handleError (Just se) = toLog $ "ERROR: " ++ show se
-}


-- That's no good, e.g.
-- typeOf (moveWindow Nothing) == typeOf (resizeWindow Nothing)
-- popHandler :: Event a => (a -> Z ()) -> Z ()
-- popHandler = (eventHooks %:=) . S.delete . EventHandler


-- pushHandler :: Event a => (a -> Z ()) -> Z ()
-- pushHandler = (eventHooks %:=) . S.insert . EventHandler


{-
dispatch :: SomeEvent -> Z ()
dispatch e = mapM_ try =<< getsL eventHooks ((++ defaultHandler) . S.toList)
    where
    try :: EventHandler (Z ()) -> Z ()
    try (EventHandler handler) = void $ whenJust (fromEvent e) handler
-}


data BaseComponent = BaseComponent
    deriving (Show, Typeable)

baseComponent :: Component
baseComponent = Component
    { componentData = BaseComponent
    , runComponent = runBaseComponent
    , onStartup = return . id
    , onShutdown = const $ return ()
    , someConsumer =
        -- [SomeHandler dispatch]
        [ EventHandler handleMapRequest
        , EventHandler handleConfigureRequest
        , EventHandler handleCirculateNotify
        ]
    -- , handleMessage = (\_ -> return ())
    }


runBaseComponent :: IO a -> BaseComponent -> IO (a, BaseComponent)
runBaseComponent f b = (,b) <$> f


-- dispatch :: SomeEvent -> StatelessZ ()
-- -- dispatch e = toLog "dispatch" >> mapM_ try defaultHandler
-- dispatch e = mapM_ try defaultHandler
--     where
--     try :: EventHandler (StatelessZ ()) -> StatelessZ ()
--     try (EventHandler handler) = whenJustM_ (fromEvent e) handler

    -- defaultHandler :: [EventHandler (StatelessZ ())]
    -- defaultHandler =
    --     -- Structure control events
    --     [ EventHandler handleMapRequest
    --     , EventHandler handleConfigureRequest
    --     , EventHandler handleCirculateNotify
    --     -- , EventHandler handleResizeRequest

        -- -- Window state notification events
        -- , EventHandler handleCreateNotify
        -- , EventHandler handleDestroyNotify
        -- , EventHandler handleMapNotify
        -- , EventHandler handleUnmapNotify

        -- -- Window crossing events
        -- , EventHandler handleEnterNotify
        -- , EventHandler handleLeaveNotify

        -- -- Input focus events
        -- , EventHandler handleFocusIn
        -- , EventHandler handleFocusOut

        -- -- Pointer events
        -- , EventHandler handleButtonPress
        -- , EventHandler handleButtonRelease

        -- -- Keyboard events
        -- , EventHandler handleKeyPress
        -- , EventHandler handleKeyRelease
        -- ]


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


-- Event handler

-- handleMapRequest :: MonadIO m => MapRequestEvent -> SetupRT m ()
-- handleMapRequest :: MonadIO m => MapRequestEvent -> Z ()

handleMapRequest :: MapRequestEvent -> StatelessZ ()
handleMapRequest e = do
    toLog "MapRequestEvent"
    connection $-> io . flip mapWindow (window_MapRequestEvent e)


handleConfigureRequest :: ConfigureRequestEvent -> StatelessZ ()
handleConfigureRequest e = do
    toLog "ConfigureRequestEvent"
    configure window values
    
    where
    window = window_ConfigureRequestEvent e
    values = copyValues e (value_mask_ConfigureRequestEvent e)


handleCirculateNotify :: CirculateNotifyEvent -> StatelessZ ()
handleCirculateNotify _ = toLog "CirculateNotifyEvent"


-- handleResizeRequest :: ResizeRequestEvent -> Z ()
-- handleResizeRequest _ = toLog "ResizeRequestEvent"
--
--
-- handleMapNotify :: MapNotifyEvent -> Z ()
-- handleMapNotify _ = toLog "MapNotifyEvent"
--
--
-- handleUnmapNotify :: UnmapNotifyEvent -> Z ()
-- handleUnmapNotify _ = toLog "UnmapNotifyEvent"
--
--
-- handleCreateNotify :: CreateNotifyEvent -> Z ()
-- -- handleCreateNotify = manage .  window_CreateNotifyEvent
-- handleCreateNotify = undefined
--
--
-- handleDestroyNotify :: DestroyNotifyEvent -> Z ()
-- -- handleDestroyNotify e = toLog "DestroyNotifyEvent" >> unmanage ( window_DestroyNotifyEvent e)
-- handleDestroyNotify e = undefined
--
--
-- handleEnterNotify :: EnterNotifyEvent -> Z ()
-- handleEnterNotify e = do
--     toLog "EnterNotifyEvent"
--     -- when (not isInferior) $ focus (event_EnterNotifyEvent e)
--     where isInferior = NotifyDetailInferior == detail_EnterNotifyEvent e
--
--
-- handleLeaveNotify :: LeaveNotifyEvent -> Z ()
-- handleLeaveNotify e = do
--     toLog "LeaveNotifyEvent"
--     -- when (not isInferior) $ unfocus (event_LeaveNotifyEvent e)
--     where isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e
--
--
-- handleFocusIn :: FocusInEvent -> Z ()
-- handleFocusIn _ = toLog "FocusInEvent"
--
--
-- handleFocusOut :: FocusOutEvent -> Z ()
-- handleFocusOut e = do
--     toLog $ "FocusOutEvent: " ++ show (mode_FocusOutEvent e)

{-
handleMapRequest :: MapRequestEvent -> Z ()
handleMapRequest e = do
    toLog "MapRequestEvent"
    connection $-> io . flip mapWindow (window_MapRequestEvent e)


handleConfigureRequest :: ConfigureRequestEvent -> Z ()
handleConfigureRequest e = undefined
-- handleConfigureRequest e = configure window values
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
-- handleCreateNotify = manage .  window_CreateNotifyEvent
handleCreateNotify = undefined


handleDestroyNotify :: DestroyNotifyEvent -> Z ()
-- handleDestroyNotify e = toLog "DestroyNotifyEvent" >> unmanage ( window_DestroyNotifyEvent e)
handleDestroyNotify e = undefined


handleEnterNotify :: EnterNotifyEvent -> Z ()
handleEnterNotify e = do
    toLog "EnterNotifyEvent"
    -- when (not isInferior) $ focus (event_EnterNotifyEvent e)
    where isInferior = NotifyDetailInferior == detail_EnterNotifyEvent e


handleLeaveNotify :: LeaveNotifyEvent -> Z ()
handleLeaveNotify e = do
    toLog "LeaveNotifyEvent"
    -- when (not isInferior) $ unfocus (event_LeaveNotifyEvent e)
    where isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e


handleFocusIn :: FocusInEvent -> Z ()
handleFocusIn _ = toLog "FocusInEvent"


handleFocusOut :: FocusOutEvent -> Z ()
handleFocusOut e = do
    toLog $ "FocusOutEvent: " ++ show (mode_FocusOutEvent e)
-}


{-
handleButtonPress :: ButtonPressEvent -> Z ()
handleButtonPress e = do
    toLog "ButtonPressEvent"
    let state = state_ButtonPressEvent e
        button = fromValue $ detail_ButtonPressEvent e
    mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)
    flip handlePress e . M.lookup (mask, button) <-$ config . buttonHandler


handleButtonRelease :: ButtonReleaseEvent -> Z ()
handleButtonRelease e = do
    toLog "ButtonReleaseEvent"
    let state = state_ButtonReleaseEvent e
        button = fromValue $ detail_ButtonReleaseEvent e
    mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)
    flip handleRelease e . M.lookup (mask, button) <-$ config . buttonHandler


isModifier :: KEYCODE -> Z Bool
isModifier keycode = asksL (config . modMask) . fold False <-$ modifierMap
    where
    fold def modmap (mask:masks) =
        let mapindex = fromValue . toBit $ mask
            keycodes = modifierToKeycode modmap mapindex
        in fold (foldr (||) def $ map (== keycode) keycodes) modmap masks
    fold def _ _ = def


ungrabKeys :: Z ()
ungrabKeys = connection $-> \c -> do
    kbdmap <- askL keyboardMap
    keys <- asksL (config . keyHandler) M.keys
    forM_ keys $ \(_, keysym) ->
        whenJust (keysymToKeycode kbdmap (fi keysym)) $ \keycode ->
            io $ ungrabKey c $ MkUngrabKey keycode (getRoot c) [ModMaskAny]


grabKeys :: Z ()
grabKeys = connection $-> \c -> do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    keys <- asksL (config . keyHandler) M.keys

    let nl = catMaybes [(fromBit . toValue) <$> keysymToModifier kbdmap modmap (fi xK_Num_Lock)]
        cl = catMaybes [(fromBit . toValue) <$> keysymToModifier kbdmap modmap (fi xK_Caps_Lock)]
        -- TODO: separate function
        combos m kc = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]
        grab (mask, keycode) = io $ grabKey c $ MkGrabKey True (getRoot c)
                                                          mask keycode
                                                          GrabModeAsync GrabModeAsync

    forM_ keys $ \(mask, keysym) ->
        whenJust (keysymToKeycode kbdmap (fi keysym)) $
            mapM_ grab . combos mask


-- KeyPressEvent:
--   isModifier  True    |  False
--   mode %:=    Manage  |  Normal
--                       |  forM handlePress keyHandler

-- KeyReleaseEvent
--   isModifier  True            |  False
--               mode == Normal  |  forM handleRelease keyHandler

-- reGrabKeys :: Z ()
-- reGrabKeys = connection $-> \c -> do
--     keys <- askL config . keyHandler

    -- forM_ keys $ \(mask, keysym) -> 

handleKeyPress2 :: KeyPressEvent -> Z ()
handleKeyPress2 e =
    isModifier (detail_KeyPressEvent e) >>= \case

    True -> do
        undefined
        -- mode %:= (\m -> if m == Normal then Manage else Normal)
        -- getL mode >>= toLog . ("KeyPressEvent: " ++) . show

    False -> do
        let state = state_KeyPressEvent e
            keycode = detail_KeyPressEvent e
            -- convert = fromBit . toBit

        modmask <- asksL (config . modMask) (L.delete ModMaskAny)

        -- when (all (`elem` state) $ convert modmask) $ mode ^:= Normal

        mask <- (\\ modmask) <$> getCleanMask state

        mapM_ (flip handlePress e)
            =<< (mapM $ asksL (config . keyHandler) . M.lookup . (mask,) . fi)
                =<< asksL keyboardMap (flip keycodeToKeysym keycode)


handleKeyRelease2 :: KeyReleaseEvent -> Z ()
handleKeyRelease2 e =
    isModifier (detail_KeyReleaseEvent e) >>= \case

    True -> do
        undefined
        -- getL mode >>= \case
        --     Normal -> do
        --         toLog "KeyReleaseEvent: Normal!"
        --         ungrabKeys
        --     Manage -> do
        --         toLog "KeyReleaseEvent: Manage!"
        --         grabKeys

    False -> do
        let state = state_KeyReleaseEvent e
            keycode = detail_KeyReleaseEvent e

        mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)

        mapM_ (flip handleRelease e)
            =<< (mapM $ asksL (config . keyHandler) . M.lookup . (mask,) . fi)
                =<< asksL keyboardMap (flip keycodeToKeysym keycode)

{-
    isModifier (detail_KeyReleaseEvent e) >>= \case
    True -> do
        toLog "KeyReleaseEvent: isModifier!"
        whenM (getsL mode (== Normal)) grabKeys

    _ -> do
        toLog "KeyReleaseEvent: not isModifier!"

        let state = state_KeyReleaseEvent e
            keycode = detail_KeyReleaseEvent e

        mask <- getL mode >>= \case
            Normal -> (\\) <$> (getCleanMask state) <*> askL (config . modMask)
            _ -> return []

        -- mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)

        let lookupKeysym keysym = M.lookup (mask, fi keysym)
        mapM_ (flip handleRelease e)
            =<< (mapM $ asksL (config . keyHandler) . lookupKeysym)
                =<< asksL keyboardMap (keycodeToKeysym keycode)
-}



    -- modmap <- askL modifierMap
    -- modmask <- askL (config . modMask)
    -- forM_ modmask $ \mask -> do
    --     let mapindex = fromValue . toBit $ mask
    --         keycodes = modifierToKeycode mapindex modmap
    --         -- is_modifier = foldr (||) False $ map (== detail_KeyReleaseEvent e) keycodes

        -- when is_modifier $ return ()

    -- forM keycodes $ \keycode 
    -- keycodes <- mapM 
    -- whenM (asks (== state_KeyPressEvent
    -- ungrabKey c $ MkUngrabKey (toValue GrabAny) (getRoot c) [ModMaskAny]


handleKeyPress :: KeyPressEvent -> Z ()
handleKeyPress e = do
    -- toLog "KeyPressEvent"
    let state = state_KeyPressEvent e
        keycode = detail_KeyPressEvent e

    mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)

    let lookupKeysym keysym = M.lookup (mask, fi keysym)
    mapM_ (flip handlePress e)
        =<< (mapM $ asksL (config . keyHandler) . lookupKeysym)
            =<< asksL keyboardMap (flip keycodeToKeysym keycode)


handleKeyRelease :: KeyReleaseEvent -> Z ()
handleKeyRelease e = do
    -- toLog "KeyReleaseEvent"
    let state = state_KeyReleaseEvent e
        keycode = detail_KeyReleaseEvent e

    mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)

    let lookupKeysym keysym = M.lookup (mask, fi keysym)
    mapM_ (flip handleRelease e)
        =<< (mapM $ asksL (config . keyHandler) . lookupKeysym)
            =<< asksL keyboardMap (flip keycodeToKeysym keycode)
-}

