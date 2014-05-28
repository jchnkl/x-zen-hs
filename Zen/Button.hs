{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             LambdaCase,
             TupleSections #-}

module Button where

import Data.Word
import Data.Maybe (catMaybes, isJust, isNothing, fromJust, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import qualified Data.List as L
import Data.Typeable
import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow (first, second)
import Control.Applicative ((<*>), (<$>))
-- import Control.Concurrent (forkIO)
import Graphics.XHB
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor

import Log
import Util
import Lens
import Types
import qualified Window as W
import Message
import Keyboard
import Component


-- | Cursors to be loaded at program startup
-- Cursors are available through @_glyphCursors@ in @Setup@
cursorGlyphs :: [Glyph]
cursorGlyphs =
    [ xC_fleur
    , xC_top_side
    , xC_top_right_corner
    , xC_top_left_corner
    , xC_bottom_side
    , xC_bottom_right_corner
    , xC_bottom_left_corner
    , xC_right_side
    , xC_left_side
    ]


buttonEventMask :: [EventMask]
buttonEventMask =
    [ EventMaskButtonMotion
    , EventMaskButtonPress
    , EventMaskButtonRelease
    ]


data PointerAction = Move | Resize | Raise | Lower
    deriving (Eq, Show, Typeable)

type ButtonMap = Map ([ModMask], ButtonIndex) PointerAction

data ButtonConfig = ButtonConfig
    { _buttonActions :: ButtonMap
    }
    deriving (Eq, Show, Typeable)

buttonActions :: (MonadIO m, Functor m) => Z m (Maybe ButtonMap)
buttonActions = (_buttonActions <$>)
            <$> asksL (config . componentConfigs) getButtonConfig


type GlyphMap = Map Glyph CURSOR

data PointerSetup = PointerSetup
    { buttonMask :: [EventMask]
    , glyphMap :: GlyphMap
    }
    deriving (Show, Typeable)


data PointerMotion = M Position
                   | R (Maybe Edge, Maybe Edge) Position Geometry
                   | MR Position (Maybe (Edge, Int)) (Maybe (Edge, Int))
                   -- | LockBorder (Maybe Int, Maybe Int)
                   -- | Lock Position (Maybe Int) (Maybe Int)
                   | Lock Position (Maybe (Edge, Int)) (Maybe (Edge, Int))
    deriving (Show, Typeable)

type PointerStack = ReaderT PointerSetup (StateT (Maybe PointerMotion) IO)


asksPS :: (PointerSetup -> a) -> Z PointerStack a
asksPS = lift . lift . asks

pointerComponent :: Component
pointerComponent = Component
    { componentData = (PointerSetup [] M.empty, Nothing)
    , runComponent = runPointerComponent
    , onStartup = startupPointerComponent
    , onShutdown = shutdownPointerComponent
    , someSinks = [ EventHandler handleButtonPress
                  , EventHandler handleMotionNotify
                  , EventHandler handleCreateNotify
                  ]
    }


runPointerComponent :: PointerStack a
                    -> (PointerSetup, Maybe PointerMotion)
                    -> IO (a, (PointerSetup, Maybe PointerMotion))
runPointerComponent f (ps, pm) = second (ps,) <$> runStateT (runReaderT f ps) pm


startupPointerComponent :: (PointerSetup, Maybe PointerMotion)
                        -> Z IO (PointerSetup, Maybe PointerMotion)
startupPointerComponent (PointerSetup bm _, p) = connection $-> \c -> do
    toLog "Button startupPointerComponent"
    glyphs <- io (withFont c "cursor" $ flip (loadGlyphCursors c) cursorGlyphs)
    buttons <- fromMaybe M.empty <$> buttonActions
    io $ putStrLn "GetClients start"

    mapM_ (grabButtons bm buttons)
        =<< maybe [] (map (^. xid) . getClientsReply) <$> sendMessage GetClients

    io $ putStrLn "GetClients done"
    return (PointerSetup buttonEventMask glyphs, p)


shutdownPointerComponent :: (PointerSetup, Maybe PointerMotion) -> Z IO ()
shutdownPointerComponent (PointerSetup _ glyphs, _) = do
    toLog "Button shutdownPointerComponent"
    connection $-> \c -> mapM_ (io . freeCursor c) (M.elems glyphs)


getButtonConfig :: [ComponentConfig] -> Maybe ButtonConfig
getButtonConfig = getConfig


handleButtonPress :: ButtonPressEvent -> Z PointerStack ()
handleButtonPress e = do
    toLog "Button ButtonPressEvent"

    mask <- (\\) <$> (getCleanMask bstate) <*> askL (config . modMask)
    flip whenJustM_ handle =<<
        (join . (M.lookup (mask, button) <$>) <$> buttonActions)

    where
    bstate = state_ButtonPressEvent e
    button = fromValue $ detail_ButtonPressEvent e

    handle :: PointerAction -> Z PointerStack ()
    handle = \case
        Move   -> doMoveResist e
        Resize -> doResize e
        Raise  -> doRaise e
        Lower  -> doLower e


doRaise :: ButtonPressEvent -> Z PointerStack ()
doRaise = W.raise . event_ButtonPressEvent

doLower :: ButtonPressEvent -> Z PointerStack ()
doLower = W.lower . event_ButtonPressEvent

doMoveResist :: ButtonPressEvent -> Z PointerStack ()
doMoveResist e = do
    doRaise e
    -- put . Just $ MR (Position event_x event_y) Nothing Nothing
    put . Just $ Lock (Position event_x event_y) Nothing Nothing
    flip whenJustM_ changeCursor =<< asksPS (M.lookup xC_fleur . glyphMap)
    where
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e


doMove :: ButtonPressEvent -> Z PointerStack ()
doMove e = do
    doRaise e
    put $ Just . M $ Position event_x event_y
    flip whenJustM_ changeCursor =<< asksPS (M.lookup xC_fleur . glyphMap)
    where
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e


{-

 algorithm for border resistance:

 find closest border within distance and direction

 if border of cclient for this direction == closest border in direction
    then
        keep cclient axis for border fixed
        lock axis
    else
        if move direction on axis is opposite
            then
                delete axis lock
                move on axis
-}

clientBorder :: Client -> Edge -> Int
clientBorder client = \case
    North -> cy
    South -> cy + fi ch
    East  -> cx + fi cw
    West  -> cx
    where cx = client ^. geometry . position . x
          cy = client ^. geometry . position . y
          cw = client ^. geometry . dimension . width
          ch = client ^. geometry . dimension . height


oppositeEdge :: Edge -> Edge
oppositeEdge = \case
    North -> South
    South -> North
    East  -> West
    West  -> East


closestBorder :: [Client] -> Client -> Edge -> Maybe Int
closestBorder clients client edge = borders
    where
    borders = result
            $ filter borderp
            $ map (flip clientBorder $ oppositeEdge edge)
            $ filter clientp
            $ clients

    result lst
        | L.null lst = Nothing
        | otherwise  = Just $ minmax lst

    borderp border
        | edge == North || edge == West = border < cb
        | edge == South || edge == East = border > cb
        where cb = clientBorder client edge

    clientp client'
        | edge == North || edge == South =
                west client >= west client' && west client <= east client'
             || east client <= east client' && east client >= west client'
        | otherwise =
                north client >= north client' && north client <= south client'
             || south client <= south client' && south client >= north client'

    minmax
        | edge == North || edge == West = maximum
        | edge == South || edge == East = minimum

    north   = cy
    south c = cy c + fi (ch c)
    east  c = cx c + fi (cw c)
    west    = cx

    cx c = c ^. geometry . position . x
    cy c = c ^. geometry . position . y
    cw c = c ^. geometry . dimension . width
    ch c = c ^. geometry . dimension . height


closestBorders :: [Client] -> Client -> [(Edge, Int)]
closestBorders cs c = catMaybes $ map cb [North, South, East, West]
    where cb e = fmap (e,) (closestBorder cs c e)


closestBordersDirection :: [(Edge, Int)]
                        -> (Maybe Edge, Maybe Edge)
                        -> (Maybe (Edge, Int), Maybe (Edge, Int))
closestBordersDirection es (e1, e2) = (e1 >>= try es, e2 >>= try es)
    where
    try []             _ = Nothing
    try ((e', b'):ebs) e | e == e' = Just (e, b')
                         | otherwise = try ebs e


directions :: Position -> Position -> (Maybe Edge, Maybe Edge)
directions from to = (x_direction delta_x, y_direction delta_y)
    where
    delta_x = from ^. x - to ^. x
    delta_y = from ^. y - to ^. y
    x_direction delta
        | delta < 0 = Just East
        | delta > 0 = Just West
        | otherwise = Nothing
    y_direction delta
        | delta > 0 = Just North
        | delta < 0 = Just South
        | otherwise = Nothing


applyBorderWidth :: Int -> (Edge, Int) -> (Edge, Int)
applyBorderWidth bw (e, b)
    | e == North || e == West = (e, b + 2 * fi bw)
    | otherwise               = (e, b - 2 * fi bw)


resist :: Client -- ^ Client about to be moved or constrained to border
       -> Int -- ^ Distance for resistance
       -> Int -- ^ Desired new position
       -> (Edge, Int) -- ^ Closest edge in direction of new position
       -> Maybe Int
resist client distance b' (e, b)
    | b == clientBorder client e && pred = Just border
    | otherwise = Nothing

    where
    cw = fi $ client ^. geometry . dimension . width
    ch = fi $ client ^. geometry . dimension . height

    pred | e == North || e == West = b - b' < distance
         | e == South              = b' - (b - ch) < distance
         | e == East               = b' - (b - cw) < distance

    border | e == North || e == West = b
           | e == South              = b - ch
           | e == East               = b - cw


moveResist :: PointerMotion -> MotionNotifyEvent -> Z PointerStack ()
moveResist (Lock ppos lock_x lock_y) e = do
    clients <- maybe [] getClientsReply <$> sendMessage GetClients
    whenJustM_ (L.find (\c -> c ^. xid == window) clients) $ flip move clients

    where
    root_x = root_x_MotionNotifyEvent e
    root_y = root_y_MotionNotifyEvent e
    window = event_MotionNotifyEvent e

    px = fi root_x - ppos ^. x
    py = fi root_y - ppos ^. y

    updateXY :: Int -> Int -> Client -> Client
    updateXY x' y' = (geometry . position . x .~ x')
                   . (geometry . position . y .~ y')

    move cclient clients = do
        -- distance <- asks (resistDistance)
        let distance = 30
        bw <- askL $ config . borderWidth

        let closest_borders = closestBordersDirection
                                  (map (applyBorderWidth $ fi bw)
                                       (closestBorders clients cclient))
                                  (directions (cclient ^. geometry . position)
                                              (Position px py))

            cx = fromMaybe px (fst closest_borders >>= resist cclient distance px)
            cy = fromMaybe py (snd closest_borders >>= resist cclient distance py)


        void (sendMessage
              (UpdateClient window $ updateXY cx cy)
                :: Z PointerStack (Maybe CoreMessageReply))

        W.configure window $ [(ConfigWindowX, fi cx), (ConfigWindowY, fi cy)]




doResize :: ButtonPressEvent -> Z PointerStack ()
doResize e = do
    doRaise e
    reply' <- io . getReply
        =<< connection $-> (io . flip getGeometry (convertXid window))
    whenRightM_ reply' $ \reply -> do
        put $ Just $ R (edges reply)
                       (Position root_x root_y)
                       (Geometry (win_pos reply) (win_dim reply))
        flip whenJustM_ changeCursor
            =<< asksPS (M.lookup (getCursor $ edges reply) . glyphMap)
    where
    window = event_ButtonPressEvent e
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e
    win_pos g = Position (fi $ x_GetGeometryReply g) (fi $ y_GetGeometryReply g)
    win_dim g = Dimension (fi $ width_GetGeometryReply g) (fi $ height_GetGeometryReply g)
    edges = getEdges . Geometry (Position event_x event_y) . win_dim


handleMotionNotify :: MotionNotifyEvent -> Z PointerStack ()
handleMotionNotify e = get >>= handle
    where
    handle :: Maybe PointerMotion -> Z PointerStack ()
    handle Nothing              = return ()
    handle (Just mr@(Lock _ _ _))       = moveResist mr e
    -- handle (Just (M p))         = W.configure window
    --     $ [(ConfigWindowX, root_x - src_x p), (ConfigWindowY, root_y - src_y p)]
    handle (Just (R edges p g)) = W.configure window
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


handleCreateNotify :: CreateNotifyEvent -> Z PointerStack ()
handleCreateNotify e = do
    toLog "Button CreateNotifyEvent"
    mask <- asksPS buttonMask
    buttons <- fromMaybe M.empty <$> buttonActions
    whenM isClient $ grabButtons mask buttons window
    where
    window = window_CreateNotifyEvent e
    isClient = maybe False isClientReply <$> sendMessage (IsClient window)


grabButtons :: MonadIO m => [EventMask] -> ButtonMap -> WindowId -> Z m ()
grabButtons eventmask actions window = connection $-> \c -> do
    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap

    forM_ (M.keys actions) $ \(m, b) -> do
        let keys = zip (combinations (m ++ modmask ++ extraModifier kbdmap modmap)) (repeat b)
        mapM_ (grab c eventmask) keys

    where
    grab c emask (mask, button) = do
        io $ grabButton c $ MkGrabButton True window emask
                            GrabModeAsync GrabModeAsync
                            (convertXid xidNone) (convertXid xidNone)
                            button mask


loadGlyphCursor :: Connection -> FONT -> Glyph -> IO CURSOR
loadGlyphCursor c font glyph = do
    cursor <- newResource c :: IO CURSOR
    createGlyphCursor c $ MkCreateGlyphCursor cursor font font
                                              (fi glyph) (fi glyph + 1)
                                              0 0 0 0xffff 0xffff 0xffff
    return cursor


loadGlyphCursors :: Connection -> FONT -> [Glyph] -> IO GlyphMap
loadGlyphCursors c font = load M.empty
    where
    load cursors (glyph:glyphs) = do
        cursor <- loadGlyphCursor c font glyph
        load (M.insert glyph cursor cursors) glyphs
    load cursors _ = return cursors


withFont :: Connection -> String -> (FONT -> IO b) -> IO b
withFont c name = bracket getFont (closeFont c)
    where
    getFont :: IO FONT
    getFont = do
        font <- newResource c :: IO FONT
        openFont c $ MkOpenFont font (fi $ length name) (stringToCList name)
        return font


changeCursor :: CURSOR -> Z PointerStack ()
changeCursor cursor = connection $-> \c -> asksPS buttonMask >>= changeGrab c
    where
    changeGrab c = liftIO . changeActivePointerGrab c
                          . MkChangeActivePointerGrab cursor
                                                      (toValue TimeCurrentTime)
