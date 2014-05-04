{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Button where


import Data.Typeable
import Control.Monad.State
import Graphics.XHB

import Log
import Util
import Lens
import Types
import Window
import SomeState

data PointerMotion = Move Position
                   | Resize (Maybe Edge, Maybe Edge) Position Geometry
    deriving (Show, Typeable)

type PointerState = LogWT (SetupRT (StateT (Maybe PointerMotion) IO))


pointerState :: SomeState
pointerState = Stateful Nothing
                      $ eventDispatcher [ EventHandler handleButtonPress
                                        , EventHandler handleMotionNotify
                                        ]


handleButtonPress :: ButtonPressEvent -> PointerState ()
handleButtonPress e = do
    toLog "ButtonPressEvent"

    case (fromValue (fi $ detail_ButtonPressEvent e :: Int)) of
        ButtonIndex1 -> put $ Just $ Move (Position root_x root_y)
        ButtonIndex2 -> do
            reply' <- io . getReply
                =<< connection $-> (io . flip getGeometry (convertXid window))
            void $ whenRight reply' $ \reply ->
                put $ Just $ Resize (edges reply)
                                    (Position root_x root_y)
                                    (Geometry (win_pos reply) (win_dim reply))
        _ -> return ()

    where
        window = event_ButtonPressEvent e
        root_x = fi $ root_x_ButtonPressEvent e
        root_y = fi $ root_y_ButtonPressEvent e
        event_x = fi $ event_x_ButtonPressEvent e
        event_y = fi $ event_y_ButtonPressEvent e
        win_pos g = Position (fi $ x_GetGeometryReply g) (fi $ y_GetGeometryReply g)
        win_dim g = Dimension (fi $ width_GetGeometryReply g) (fi $ height_GetGeometryReply g)
        edges = getEdges . Geometry (Position event_x event_y) . win_dim


handleMotionNotify :: MotionNotifyEvent -> PointerState ()
handleMotionNotify e = get >>= handle
    where
    handle :: Maybe PointerMotion -> PointerState ()
    handle Nothing                   = return ()
    handle (Just (Move p))           = configure window
        $ [(ConfigWindowX, root_x - src_x p), (ConfigWindowY, root_y - src_y p)]
    handle (Just (Resize edges p g)) = configure window
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
