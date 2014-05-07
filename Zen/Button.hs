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


type PointerState = StateT (Maybe PointerMotion) IO


pointerComponent :: Component
pointerComponent = Component
    { component = Nothing
    , runComponent = runPointerComponent
    , initialize = return ()
    , terminate = return ()
    , handleEvent = eventDispatcher [ EventHandler handleButtonPress
                                    , EventHandler handleMotionNotify
                                    , EventHandler handleCreateNotify
                                    ]
    , handleMessage = (\_ -> return ())
    }


runPointerComponent :: PointerState a
                    -> Maybe PointerMotion
                    -> IO (a, Maybe PointerMotion)
runPointerComponent = runStateT


getButtonConfig :: [ComponentConfig] -> Maybe ButtonConfig
getButtonConfig = getConfig


handleButtonPress :: ButtonPressEvent -> Z PointerState ()
handleButtonPress e = do
    toLog "ButtonPressEvent"

    mask <- (\\) <$> (getCleanMask state) <*> askL (config . modMask)
    void $ flip whenJust handle =<<
        ((M.lookup (mask, button) . buttonActions =<<) . getButtonConfig)
            <$> askL (config . componentConfigs)

    where
    state = state_ButtonPressEvent e
    button = fromValue $ detail_ButtonPressEvent e

    handle :: PointerAction -> Z PointerState ()
    handle = \case
        Move   -> doMove e
        Resize -> doResize e
        Raise  -> doRaise e
        Lower  -> doLower e


doRaise :: ButtonPressEvent -> Z PointerState ()
doRaise = raise . event_ButtonPressEvent

doLower :: ButtonPressEvent -> Z PointerState ()
doLower = lower . event_ButtonPressEvent

doMove :: ButtonPressEvent -> Z PointerState ()
doMove e = do
    doRaise e
    put $ Just $ M (Position root_x root_y)
    where
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e


doResize :: ButtonPressEvent -> Z PointerState ()
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


handleMotionNotify :: MotionNotifyEvent -> Z PointerState ()
handleMotionNotify e = get >>= handle
    where
    handle :: Maybe PointerMotion -> Z PointerState ()
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


handleCreateNotify :: CreateNotifyEvent -> Z PointerState ()
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    grabButtons $ window_CreateNotifyEvent e


grabButtons :: WindowId -> Z PointerState ()
grabButtons window = connection $-> \c -> whenM (isClient window) $ do
    toLog $ "grabButtons for " ++ show window

    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    eventmask <- askL buttonMask

    buttons <- asksL (config . componentConfigs)
                     (fromMaybe [] . fmap (M.keys . buttonActions) . getButtonConfig)

    forM_ buttons $ \(m, b) -> do
        let keys = zip (combinations (m ++ modmask ++ extraModifier kbdmap modmap)) (repeat b)
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
