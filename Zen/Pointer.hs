module Pointer where

import Control.Monad (void)
import Control.Monad.State
import Graphics.XHB
import Graphics.X11.Xlib.Cursor (xC_fleur)

import Log
import Lens
import Util
import Event
import Window
-- import Cursor
import Types


resizeWindowHandler :: InputEventHandler ButtonPressEvent ButtonReleaseEvent
resizeWindowHandler = InputHandler { press = onPress, release = onRelease }
    where
    onPress :: ButtonPressEvent -> Z ()
    onPress e = do
        toLog "Press ButtonIndex2"
        let window = event_ButtonPressEvent e
            root_x = fi $ root_x_ButtonPressEvent e
            root_y = fi $ root_y_ButtonPressEvent e
            event_x = fi $ event_x_ButtonPressEvent e
            event_y = fi $ event_y_ButtonPressEvent e
            geom_x = fi . x_GetGeometryReply
            geom_y = fi . y_GetGeometryReply
            geom_w = fi . width_GetGeometryReply
            geom_h = fi . height_GetGeometryReply
            pos g = Position (geom_x g) (geom_y g)
            dim g = Dimension (geom_w g) (geom_h g)
            edges = getEdges . Geometry (Position event_x event_y) . dim
            handler g = resizeWindow $ Just (edges g,
                                             Position root_x root_y,
                                             Geometry (pos g) (dim g))

        raise window

        reply' <- io . getReply
            =<< connection $-> (io . flip getGeometry (convertXid window))

        void $ whenRight reply' $ \reply -> do
            io $ putStrLn $ "event: " ++ show e ++ "\n"
            io $ putStrLn $ "reply: " ++ show reply ++ "\n"
            io $ putStrLn $ "edges: " ++ show (edges reply) ++ "\n"
            pushHandler $ handler reply
            lookupCursor (getCursor $ edges reply) >>= changeCursor

    onRelease :: ButtonReleaseEvent -> Z ()
    onRelease = const $ popHandler $ resizeWindow Nothing


moveWindowHandler :: InputEventHandler ButtonPressEvent ButtonReleaseEvent
moveWindowHandler = InputHandler { press = onPress, release = onRelease }
    where
    onPress :: ButtonPressEvent -> Z ()
    onPress e = do
        toLog "Press ButtonIndex1"
        let window = event_ButtonPressEvent e
            event_x = event_x_ButtonPressEvent e
            event_y = event_y_ButtonPressEvent e
            pos = Position (fi event_x) (fi event_y)

        raise window
        pushHandler $ moveWindow $ Just pos
        lookupCursor xC_fleur >>= changeCursor

    onRelease :: ButtonReleaseEvent -> Z ()
    onRelease = const $ popHandler $ moveWindow Nothing


raiseWindowHandler :: InputEventHandler ButtonPressEvent ButtonReleaseEvent
raiseWindowHandler = mkInputHandler { press = raise . event_ButtonPressEvent }


lowerWindowHandler :: InputEventHandler ButtonPressEvent ButtonReleaseEvent
lowerWindowHandler = mkInputHandler { press = lower . event_ButtonPressEvent }
