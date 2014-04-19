module Event where

import qualified Data.Map as M
import Data.Word
import Data.Maybe
import Control.Monad
import Graphics.XHB
import Control.Monad.IO.Class (liftIO)

import Util
import Types
import Client


handler :: [EventHandler (Z Bool)]
handler =
    [ EventHandler handleCreateNotify
    , EventHandler handleDestroyNotify
    , EventHandler handleMapRequest
    , EventHandler handleConfigureRequest
    , EventHandler handleButtonPress
    , EventHandler handleButtonRelease
    ]


handleError :: Maybe SomeError -> Z ()
handleError Nothing = return ()
handleError (Just se) = toLog $ "ERROR: " ++ show se


dispatch :: SomeEvent -> Z ()
dispatch e = getsL (eventHandler <.> config) >>= runHandler
    where
    runHandler :: [EventHandler (Z Bool)] -> Z ()
    runHandler (EventHandler h:hs) = do
        taken <- fromMaybe (return False) $ liftM h $ fromEvent e
        unless taken $ runHandler hs
    runHandler _ = return ()


handleCreateNotify :: CreateNotifyEvent -> Z Bool
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    insertWindow window
    grabButtons window
    return True
    where window = window_CreateNotifyEvent e


handleDestroyNotify :: DestroyNotifyEvent -> Z Bool
handleDestroyNotify e = do
    toLog "DestroyNotifyEvent"
    removeWindow (window_DestroyNotifyEvent e) >> return True


handleMapRequest :: MapRequestEvent -> Z Bool
handleMapRequest e = do
    toLog "MapRequestEvent"
    liftIO $ print e
    withConnection $ liftIO . flip mapWindow event_window
    return True
    where
    event_window = window_MapRequestEvent e


handleConfigureRequest :: ConfigureRequestEvent -> Z Bool
handleConfigureRequest e = do
    toLog "ConfigureRequestEvent"
    c <- asksL connection
    liftIO $ do
        print e
        configureWindow c $ MkConfigureWindow win mask values
    liftIO (pollForError c) >>= handleError
    return True
    where
    win = window_ConfigureRequestEvent e
    mask = toMask $ value_mask_ConfigureRequestEvent e
    values = toValueParam $ copyValues (value_mask_ConfigureRequestEvent e)

    copyValues :: [ConfigWindow] -> [(ConfigWindow, Word32)]
    copyValues (ConfigWindowX           : ms) = (ConfigWindowX,           fi $ x_ConfigureRequestEvent e)                    : copyValues ms
    copyValues (ConfigWindowY           : ms) = (ConfigWindowY,           fi $ y_ConfigureRequestEvent e)                    : copyValues ms
    copyValues (ConfigWindowWidth       : ms) = (ConfigWindowWidth,       fi $ width_ConfigureRequestEvent e)                : copyValues ms
    copyValues (ConfigWindowHeight      : ms) = (ConfigWindowHeight,      fi $ height_ConfigureRequestEvent e)               : copyValues ms
    copyValues (ConfigWindowBorderWidth : ms) = (ConfigWindowBorderWidth, fi $ border_width_ConfigureRequestEvent e)         : copyValues ms
    copyValues (ConfigWindowSibling     : ms) = (ConfigWindowSibling,     convertXid $ sibling_ConfigureRequestEvent e) : copyValues ms
    copyValues (ConfigWindowStackMode   : ms) = (ConfigWindowStackMode,   toValue $ stack_mode_ConfigureRequestEvent e)      : copyValues ms
    copyValues _ = []


handleButtonPress :: ButtonPressEvent -> Z Bool
handleButtonPress e = do
    toLog "ButtonPressEvent"
    mask <- getsL (modMask <.> config)
    if toBit mask `notElem` map toBit (state_ButtonPressEvent e)
        then return False
        else do
            fs <- getsL (buttonPressHandler <.> config)
            void $ whenJust (M.lookup (fromValue $ detail_ButtonPressEvent e) fs) $ \f -> f e
            return True


handleButtonRelease :: ButtonReleaseEvent -> Z Bool
handleButtonRelease e = do
    toLog "ButtonReleaseEvent"
    mask <- getsL (modMask <.> config)
    if toBit mask `notElem` map toBit (state_ButtonReleaseEvent e)
        then return False
        else do
            fs <- getsL (buttonReleaseHandler <.> config)
            void $ whenJust (M.lookup (fromValue $ detail_ButtonReleaseEvent e) fs) $ \f -> f e
            return True


moveWindow :: MotionNotifyEvent -> Z Bool
moveWindow e = do
    withConnection $ \c -> getsL pointer >>= configure c window
    return True
    where
    root_x = fi $ root_x_MotionNotifyEvent e
    root_y = fi $ root_y_MotionNotifyEvent e
    window = event_MotionNotifyEvent e
    configure c w (Position x' y') = liftIO $ configureWindow c $
        MkConfigureWindow w (toMask [ConfigWindowX, ConfigWindowY])
                            (toValueParam [(ConfigWindowX, root_x - fi x'),
                                           (ConfigWindowY, root_y - fi y')])


resizeWindow :: MotionNotifyEvent -> Z Bool
resizeWindow e = do
    toLog "resizeWindow"
    -- TODO: do this with focused client
    void $ withConnection $ withClient window . configure

    return True

    where
    window = event_MotionNotifyEvent e
    newx = fi (root_x_MotionNotifyEvent e)
    newy = fi (root_y_MotionNotifyEvent e)

    configure c (Client _ (Geometry _ (Dimension w' h'))) = do
        Position oldx oldy <- getsL pointer
        let neww = fi w' + fi (newx - oldx)
            newh = fi h' + fi (newy - oldy)
        toLog $ "resize to " ++ show neww ++ "x" ++ show newh
        liftIO $ configureWindow c $
            MkConfigureWindow window
                              (toMask [ConfigWindowWidth, ConfigWindowHeight])
                              (toValueParam [(ConfigWindowWidth, neww),
                                             (ConfigWindowHeight, newh)])


defaultButtonPressHandler :: ButtonPressHandler
defaultButtonPressHandler = M.fromList
    [ (ButtonIndex1, \e -> do
            toLog "Press ButtonIndex1"
            let window = event_ButtonPressEvent e
                event_x = event_x_ButtonPressEvent e
                event_y = event_y_ButtonPressEvent e

            raise window
            pointer ^:= Position (fi event_x) (fi event_y)
            eventHandler <.> config %:= (EventHandler moveWindow :)
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
            void $ flip whenRight update =<< liftIO . getReply =<<
                withConnection (liftIO . flip getGeometry (convertXid window))
            eventHandler <.> config %:= (EventHandler resizeWindow :)
      )
    ]


defaultButtonReleaseHandler :: ButtonReleaseHandler
defaultButtonReleaseHandler = M.fromList
    [ (ButtonIndex1, const $ do
        toLog "Release ButtonIndex1"
        eventHandler <.> config %:= drop 1
      )
    , (ButtonIndex3, const $ do
        toLog "Release ButtonIndex2")
    , (ButtonIndex2, const $ do
        toLog "Release ButtonIndex3"
        eventHandler <.> config %:= drop 1
      )
    ]
