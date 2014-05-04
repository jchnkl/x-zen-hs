{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, LambdaCase #-}

module Button where


import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import Data.Typeable
import Control.Monad.State hiding (state)
import Control.Applicative ((<*>), (<$>))
import Graphics.XHB

import Log
import Util
import Lens
import Types
import Window
import Keyboard
import Component


data ButtonConfig = ButtonConfig
    { buttonActions :: Map ([ModMask], ButtonIndex) PointerAction }
    deriving (Eq, Show, Typeable)


data PointerAction = Move | Resize | Raise | Lower
    deriving (Eq, Show, Typeable)


data PointerMotion = M Position
                   | R (Maybe Edge, Maybe Edge) Position Geometry
    deriving (Show, Typeable)

type PointerState = LogWT (SetupRT (StateT (Maybe PointerMotion) IO))


pointerState :: Component
pointerState = Stateful
    { initState = return ()
    , someState = Nothing
    , stateHandler = eventDispatcher [ EventHandler handleButtonPress
                                     , EventHandler handleMotionNotify
                                     , EventHandler handleCreateNotify
                                     ]
    }


getButtonConfig :: [ComponentConfig] -> Maybe ButtonConfig
getButtonConfig = getConfig

-- getButtonConfig :: [ComponentConfig] -> Maybe ButtonConfig
-- getButtonConfig (ComponentConfig c:cs) = case ((cast c) :: Maybe ButtonConfig) of
--     Just c' -> Just c' :: Maybe ButtonConfig
--     Nothing -> getConfig cs
-- getButtonConfig  _ = Nothing


handleButtonPress :: ButtonPressEvent -> PointerState ()
handleButtonPress e = do
    toLog "ButtonPressEvent"

    mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)
    void $ flip whenJust handle =<<
        ((M.lookup (mask, button) . buttonActions =<<) . getButtonConfig)
            <$> askL (config . componentConfigs)

    where
    state = state_ButtonPressEvent e
    button = fromValue $ detail_ButtonPressEvent e

    handle :: PointerAction -> PointerState ()
    handle = \case
        Move   -> doMove e
        Resize -> doResize e
        Raise  -> doRaise e
        Lower  -> doLower e


doRaise :: ButtonPressEvent -> PointerState ()
doRaise = raise . event_ButtonPressEvent

doLower :: ButtonPressEvent -> PointerState ()
doLower = lower . event_ButtonPressEvent

doMove :: ButtonPressEvent -> PointerState ()
doMove e = do
    doRaise e
    put $ Just $ M (Position root_x root_y)
    where
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e


doResize :: ButtonPressEvent -> PointerState ()
doResize e = do
    doRaise e
    reply' <- io . getReply
        =<< connection $-> (io . flip getGeometry (convertXid window))
    void $ whenRight reply' $ \reply ->
        put $ Just $ R (edges reply)
                            (Position root_x root_y)
                            (Geometry (win_pos reply) (win_dim reply))
    where
    window = event_ButtonPressEvent e
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e
    win_pos g = Position (fi $ x_GetGeometryReply g) (fi $ y_GetGeometryReply g)
    win_dim g = Dimension (fi $ width_GetGeometryReply g) (fi $ height_GetGeometryReply g)
    edges = getEdges . Geometry (Position event_x event_y) . win_dim


-- handleButtonPress :: ButtonPressEvent -> Z ()
-- handleButtonPress e = do
--     toLog "ButtonPressEvent"
--     flip handlePress e . M.lookup (mask, button) <-$ config . buttonHandler

    -- handlePress :: Maybe (InputEventHandler pe re) -> pe -> Z ()
    -- handlePress Nothing _ = return ()
    -- handlePress (Just h) e = dispatchPress h e



handleMotionNotify :: MotionNotifyEvent -> PointerState ()
handleMotionNotify e = get >>= handle
    where
    handle :: Maybe PointerMotion -> PointerState ()
    handle Nothing                   = return ()
    handle (Just (M p))           = configure window
        $ [(ConfigWindowX, root_x - src_x p), (ConfigWindowY, root_y - src_y p)]
    handle (Just (R edges p g)) = configure window
        $ (values (fst edges) p g) ++ (values (snd edges) p g)

    window = event_MotionNotifyEvent e
    root_x = fi $ root_x_MotionNotifyEvent e
    root_y = fi $ root_y_MotionNotifyEvent e
    src_x = fi . (^. x)
    src_y = fi . (^. y)
    win_x = fi . (^. position . x)
    win_y = fi . (^. position . y)
    win_w = fi . (^. dimension . width)
    win_h = fi . (^. dimension . height)
    delta_x p = root_x - src_x p
    delta_y p = root_y - src_y p

    values edge p g = case edge of
        Just North -> [(ConfigWindowY, win_y g + delta_y p),
                       (ConfigWindowHeight, win_h g - delta_y p)]
        Just South -> [(ConfigWindowHeight, win_h g + delta_y p)]
        Just East  -> [(ConfigWindowWidth, win_w g + delta_x p)]
        Just West  -> [(ConfigWindowX, win_x g + delta_x p),
                       (ConfigWindowWidth, win_w g - delta_x p)]
        _          -> []



handleCreateNotify :: CreateNotifyEvent -> PointerState ()
-- handleCreateNotify = grabButtons . window_CreateNotifyEvent
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    grabButtons $ window_CreateNotifyEvent e


grabButtons :: WindowId -> PointerState ()
grabButtons window = connection $-> \c -> whenM (isClient window) $ do
    toLog $ "grabButtons for " ++ show window

    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    eventmask <- askL buttonMask

    -- buttons <- asksL (config . buttonHandler) (M.keys)
    -- askL (config . componentConfigs) >>= toLog . show
    asksL (config . componentConfigs) getButtonConfig >>= toLog . ("buttonConfig: " ++) . show

    -- buttons <- asksL (config . buttonHandler) (M.keys)
    buttons <- asksL (config . componentConfigs)
                     (fromMaybe [] . fmap (M.keys . buttonActions) . getButtonConfig)

    -- let modifier = combinations (m ++ extraModifier kbdmap modmap)

    -- -- [1,2,3] -> [[1],[1,2],[1,3],[2],[2,3],[3]]
    -- combinations :: [a] -> [[a]]

    toLog $ "buttons: " ++ show buttons

    forM_ buttons $ \(m, b) -> do
        -- let modifier = combinations (m ++ modmask ++ extraModifier kbdmap modmap)
        let keys = zip (combinations (m ++ modmask ++ extraModifier kbdmap modmap)) (repeat b)
        -- toLog $ "modifier: " ++ show modifier
        toLog $ "combinations: " ++ show keys
        mapM_ (grab c eventmask) keys

    where
    grab c eventmask (mask, button) = do
        toLog $ "grabbing: " ++ show (mask, button)
        io $ grabButton c $ MkGrabButton True window eventmask
                            GrabModeAsync GrabModeAsync
                            (convertXid xidNone) (convertXid xidNone)
                            button mask


isClient :: (Functor m, MonadIO m) => WindowId -> Z m Bool
isClient window = check <$> attributes
    where
    attributes :: MonadIO m => Z m (Either SomeError GetWindowAttributesReply)
    attributes = io . getReply =<<
        io . flip getWindowAttributes window <-$ connection

    check :: Either SomeError GetWindowAttributesReply -> Bool
    check (Right reply) = not $ isUnviewable reply
    check _             = False

    isUnviewable :: GetWindowAttributesReply -> Bool
    isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r
