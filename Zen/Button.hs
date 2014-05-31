{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             LambdaCase,
             TupleSections #-}

module Button where

import Data.Word
import Data.Maybe (isJust, fromJust, catMaybes, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import qualified Data.List as L
import Data.Typeable
import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***), first, second)
import Control.Applicative ((<*>), (<$>))
-- import Control.Concurrent (forkIO)
import Graphics.XHB
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor

import Log
import Util
import Lens
import Types
import qualified Core as C
import qualified Window as W
import Message
import Keyboard (getCleanMask, extraModifier)
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

data PointerOptions = Resist Int
    deriving (Eq, Show, Typeable)

data PointerAction = Move
                   | Resize
                   | Raise
                   | Lower
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


data PointerMotion = M Position Position
                   | R (Maybe Direction, Maybe Direction) Position Geometry
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
    , someSinks = const $ [ EventHandler handleButtonPress
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

    -- mapM_ (grabButtons bm buttons)
    --     =<< maybe [] (map (^. xid) . getClientsReply) <$> sendMessage GetClients

    C.withQueueM $ mapM_ (grabButtons bm buttons) . M.keys

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
        (Move) -> doMove e
        (Resize)    -> doResize e
        (Raise)     -> doRaise e
        (Lower)     -> doLower e


doRaise :: ButtonPressEvent -> Z PointerStack ()
doRaise = W.raise . event_ButtonPressEvent


doLower :: ButtonPressEvent -> Z PointerStack ()
doLower = W.lower . event_ButtonPressEvent


doMove :: ButtonPressEvent -> Z PointerStack ()
doMove e = do
    doRaise e
    put . Just $ M (Position event_x event_y) (Position root_x root_y)
    -- put $ Just . M $ Position root_x root_y
    flip whenJustM_ changeCursor =<< asksPS (M.lookup xC_fleur . glyphMap)
    where
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e


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


moveMotionNotify :: MotionNotifyEvent -> Z PointerStack ()
moveMotionNotify e = get >>= \case
    Just m -> do
        let bw = 3
        mclient <- join . fmap getClientReply <$> sendMessage (GetClient window)
        clients <- maybe [] (map (addbw bw) . M.elems . getQueueReply) <$> sendMessage GetQueue
        whenJustM_ mclient $ doMoveMotionNotify e m clients . addbw bw
    _ -> return ()
    where window = event_MotionNotifyEvent e
          addbw bw = -- (geometry . position . x -~ bw)
                   -- . (geometry . position . y -~ bw)
                     (geometry . dimension . width +~ 2 * bw)
                   . (geometry . dimension . height +~ 2 * bw)


doMoveMotionNotify e (M epos rpos) clients client = do

    -- let npos = Position (fi root_x - ppos ^. x) (fi root_y - ppos ^. y)
    let abs_pos = Position (fi root_x) (fi root_y) - epos
    let rel_pos = Position (fi root_x) (fi root_y) - rpos

    let bw = 6
        dist = 50
        snap_key = KeyButMaskShift
        want_snap = snap_key `elem` state_MotionNotifyEvent e
        cgeometry = client ^. geometry
        cgeometries = cgeometry `L.delete` map (^. geometry) clients

    toLog . ("client: " ++) $ show client
    toLog . ("doSnap2: " ++) . show $ doSnap2 bw dist cgeometries cgeometry rel_pos

    let dirs = directionsFromRelative rel_pos
    toLog $ ("directionsFromRelative: " ++) . show $ dirs

    let applyCorrection (d, b) = (d, correction d cgeometry b)
    let snapped_borders = first (fmap applyCorrection)
                        $ second (fmap applyCorrection)
                        $ snapBorders cgeometry cgeometries

    toLog . ("snapBorders: " ++) . show $ snapped_borders

    let check_snap b = if want_snap then Just b else Nothing
    let snap_pos = join (***) (>>= check_snap)
                 $ doSnap2 bw dist cgeometries cgeometry rel_pos

        -- do_snap (x, y) = (x >>= check_snap, y >>= check_snap)

    let msx = fst snap_pos
        msy = snd snap_pos
        ax = abs_pos ^. x
        ay = abs_pos ^. y

    let
        checkpos _   def Nothing   = def
        checkpos dir _   (Just db) = checkdir db dir

        checkdir (d, b) Nothing    = Just b
        checkdir (d, b) (Just dir) = if d == dir then Just b else Nothing

        checkdelta def delta b = if delta > dist then def else b


    -- let mx = checkpos (fst dirs) (fst snapped_borders) (fst snap_pos)
    let my = checkpos (snd dirs) (snd snap_pos) (snd snapped_borders)

    -- let mx = case fst snapped_borders of
    --         Just (d, b) -> case fst dirs of
    --             Just xdir -> if d == xdir then Just b else Nothing
    --             _ -> Just b
    --         -- _ -> Nothing
    --         _ -> msx

    -- let my = case snd snapped_borders of
    --         Just (d, b) -> case snd dirs of
    --             Just ydir -> if d == ydir then Just b else Nothing
    --             _ -> Just b
    --         _ -> msy
    --         -- _ -> Nothing


    let delta = abs $ epos - Position (fi $ event_x) (fi $ event_y)

    -- let px = if (delta ^. x) > dist then ax else fromMaybe ax mx
    -- let py = if (delta ^. y) > dist then ay else fromMaybe ay my

    let px = fromMaybe ax $ fmap (checkdelta ax (delta ^. x))
                          $ checkpos (fst dirs) (fst snap_pos) (fst snapped_borders)

    let py = fromMaybe ay $ fmap (checkdelta ay (delta ^. y)) my

    put . Just $ M epos $ Position (fi root_x) (fi root_y)

    sendMessage_ $ ModifyClient window $ changePosition
                 $ Position px py

    W.configure window [(ConfigWindowX, fi px), (ConfigWindowY, fi py)]

    -- sendMessage_ $ ModifyClient window $ updatePosition nx ny

    toLog $ ("root_x: " ++) . show $ root_x
    toLog $ ("root_y: " ++) . show $ root_y
    toLog $ ("abs_pos: " ++) . show $ abs_pos
    toLog $ ("rel_pos: " ++) . show $ rel_pos
    toLog $ ("snap_pos: " ++) . show $ snap_pos
    toLog $ ("delta: " ++) . show $ delta


    where
    root_x = root_x_MotionNotifyEvent e
    root_y = root_y_MotionNotifyEvent e
    event_x = event_x_MotionNotifyEvent e
    event_y = event_y_MotionNotifyEvent e
    window = event_MotionNotifyEvent e

    changePosition :: Position -> Client -> Client
    changePosition p = geometry . position .~ p

    updatePosition :: Maybe Int -> Maybe Int -> Client -> Client
    updatePosition (Just x')   Nothing = geometry . position . x .~ x'
    updatePosition Nothing   (Just y') = geometry . position . y .~ y'
    updatePosition (Just x') (Just y') = (geometry . position . x .~ x')
                                       . (geometry . position . y .~ y')
    updatePosition _     _             = id

    makeSnapPosition :: Maybe (Maybe Border, Maybe Border) -> Position
    makeSnapPosition (Just (Just x', Nothing)) = Position x' 0
    makeSnapPosition (Just (Nothing, Just y')) = Position 0  y'
    makeSnapPosition (Just (Just x', Just y')) = Position x' y'
    makeSnapPosition _                         = nullPosition

    modifyPosition :: Position -> Client -> Client
    modifyPosition (Position x' y') = (geometry . position . x +~ x')
                                    . (geometry . position . y +~ y')

    configValues :: Maybe Int -> Maybe Int -> [(ConfigWindow, Word32)]
    configValues (Just x')   Nothing = [(ConfigWindowX, fi x')]
    configValues Nothing   (Just y') = [(ConfigWindowY, fi y')]
    configValues (Just x') (Just y') = [(ConfigWindowX, fi x'), (ConfigWindowY, fi y')]
    configValues _         _         = []

    -- configValues :: Position -> Position -> [(ConfigWindow, Word32)]
    -- configValues abs_p rel_p = cx ++ cy
    --     where
    --     cx = if rel_p ^. x /= 0 then [(ConfigWindowX, fi $ abs_p ^. x)] else []
    --     cy = if rel_p ^. y /= 0 then [(ConfigWindowY, fi $ abs_p ^. y)] else []
    --     -- [(ConfigWindowX, fi $ p ^. x), (ConfigWindowY, fi $ p ^. y)]

    w = fromXid $ toXid (0 :: Word32) :: WindowId
    warp x' y' = do
        -- toLog $ "warp to " ++ show p
        connection $-> \c -> io $ warpPointer c
            $ MkWarpPointer w (getRoot c) 0 0 0 0 (fi x') (fi y')
            --
            -- $ MkWarpPointer w (getRoot c) 0 0 0 0 (fi x) (fi y)
        -- $ MkWarpPointer window window root_x root_y 0 0 (root_x + fi x) (root_y + fi y)
        -- $ MkWarpPointer window w 0 0 0 0 (root_x + fi x) (root_y + fi y)

clientBorder :: Client -> Direction -> Int
clientBorder client = \case
    North -> cy
    South -> cy + fi ch
    East  -> cx + fi cw
    West  -> cx
    where cx = client ^. geometry . position . x
          cy = client ^. geometry . position . y
          cw = client ^. geometry . dimension . width
          ch = client ^. geometry . dimension . height


oppositeDirection :: Direction -> Direction
oppositeDirection = \case
    North -> South
    South -> North
    East  -> West
    West  -> East


closestBorder :: [Client] -> Client -> Direction -> Maybe Int
closestBorder clients client edge = borders
    where
    borders = result
            $ filter borderp
            $ map (flip clientBorder $ oppositeDirection edge)
            $ filter clientp
            $ clients

    result lst
        | L.null lst = Nothing
        | otherwise  = Just $ minmax lst

    borderp border
        | edge == North || edge == West = border < cb
        | edge == South || edge == East = border > cb
        | otherwise                     = False
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
        | otherwise                     = head

    north   = cy
    south c = cy c + fi (ch c)
    east  c = cx c + fi (cw c)
    west    = cx

    cx c = c ^. geometry . position . x
    cy c = c ^. geometry . position . y
    cw c = c ^. geometry . dimension . width
    ch c = c ^. geometry . dimension . height


closestBorders :: [Client] -> Client -> [(Direction, Int)]
closestBorders cs c = catMaybes $ map cb [North, South, East, West]
    where cb e = fmap (e,) (closestBorder cs c e)


closestBordersDirection :: [(Direction, Int)]
                        -> (Maybe Direction, Maybe Direction)
                        -> (Maybe (Direction, Int), Maybe (Direction, Int))
closestBordersDirection es (e1, e2) = (e1 >>= try es, e2 >>= try es)
    where
    try []             _ = Nothing
    try ((e', b'):ebs) e | e == e' = Just (e, b')
                         | otherwise = try ebs e


directionsFromRelative :: Position -> (Maybe Direction, Maybe Direction)
directionsFromRelative p = (x_edge, y_edge)
    where
    x_edge
        | (p ^. x) > 0 = Just East
        | (p ^. x) < 0 = Just West
        | otherwise    = Nothing
    y_edge
        | (p ^. y) < 0 = Just North
        | (p ^. y) > 0 = Just South
        | otherwise    = Nothing


directions :: Position -> Position -> (Maybe Direction, Maybe Direction)
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


cropBorder :: (Integral a, Num a) => a -> (Direction, Int) -> (Direction, Int)
cropBorder bw (e, b)
    | e == North || e == West = (e, b + 2 * fi bw)
    | otherwise               = (e, b - 2 * fi bw)


resist :: Client -- ^ Client to be moved or resized
       -> Int -- ^ Distance for resistance
       -> Int -- ^ Desired new position
       -> (Direction, Int) -- ^ Closest edge in direction of new position
       -> Maybe Int -- ^ Maybe a border for resistance
resist client distance b' (e, b)
    | b == clientBorder client e && pred = Just border
    | otherwise = Nothing

    where
    cw = fi $ client ^. geometry . dimension . width
    ch = fi $ client ^. geometry . dimension . height

    pred | e == North || e == West = b - b' < distance
         | e == South              = b' - (b - ch) < distance
         | e == East               = b' - (b - cw) < distance
         | otherwise               = False

    border | e == North || e == West = b
           | e == South              = b - ch
           | e == East               = b - cw
           | otherwise               = b


doResist' :: WindowId -> Position -> Z PointerStack Position
doResist' window pos = do
    let distance = 50
    bwidth <- askL $ config . borderWidth
    clients <- maybe [] (M.elems . getQueueReply) <$> sendMessage GetQueue

    mclient <- ((getClientReply =<<) <$> sendMessage (GetClient window))
    whenJustM_ mclient $ \client -> do
        let dirs = directions (client ^. geometry . position) pos
        toLog $ "doResist' dirs: " ++ show dirs
        let closest_borders = map (cropBorder bwidth) $ closestBorders clients client
        toLog $ "doResist' closest_borders: " ++ show closest_borders
        let closest_borders_dir = closestBordersDirection closest_borders dirs
        toLog $ "doResist' closest_borders_dir: " ++ show closest_borders_dir

    getResistance clients bwidth distance <$> sendMessage (GetClient window)
    where
    getResistance cs bw d = fromMaybe pos
                          . (fmap $ \client -> doResist cs client (fi bw) d pos)
                          . (getClientReply =<<)


doResist :: [Client] -- ^ All clients which are candidates for resistance
         -> Client -- ^ The client to be moved
         -> Int -- ^ Border width
         -> Int -- ^ Resistance distance
         -> Position -- ^ New position for client
         -> Position -- ^ Either a position with resistance or just the new position
doResist clients client bw distance pos = final_position closest_borders_dir
    where
    dirs = directions (client ^. geometry . position) pos
    closest_borders = map (cropBorder bw) $ closestBorders clients client
    closest_borders_dir = closestBordersDirection closest_borders dirs
    px = pos ^. x
    py = pos ^. y
    final_position (ex, ey) = Position
        (fromMaybe px $ ex >>= resist client distance px)
        (fromMaybe py $ ey >>= resist client distance py)


snap :: -- Client -- ^ Client to be moved or resized
        Int -- ^ Distance for snapping
     -> Int -- ^ Desired new position
     -> (Direction, Int) -- ^ Closest edge in direction of new position
     -> Maybe Int -- ^ Maybe a border to snap on
snap distance b' (e, b)
    | pred = Just b -- border
    | otherwise = Nothing

    where
    -- cw = fi $ client ^. geometry . dimension . width
    -- ch = fi $ client ^. geometry . dimension . height

    pred | e == North || e == West = -- let d = b - b' in d < distance
                                     -- let d = b' -b in d > 0 && d < distance
                                     b' - b < distance && b' - b > 0
         | e == South || e == East = b - b' < distance && b - b' > 0
         -- | e == East               = b' - (b - cw) < distance
         | otherwise               = False

    -- border | e == North || e == West = b
    --        | e == South              = b - ch
    --        | e == East               = b - cw
    --        | otherwise               = b


doSnap' :: WindowId -> Position -> Z PointerStack Position
doSnap' window pos = do
    let distance = 50
    bwidth <- askL $ config . borderWidth
    clients <- maybe [] (M.elems . getQueueReply) <$> sendMessage GetQueue
    getSnap clients bwidth distance <$> sendMessage (GetClient window)
    where
    getSnap cs bw d = fromMaybe pos
                    . (fmap $ \client -> doSnap cs client (fi bw) d pos)
                    . (getClientReply =<<)


{-
 snap algorithm:

 find closests borders in moving direction

 when closest border for a direction is within snap distance, move window there

 -}


doSnap :: [Client] -- ^ All clients which are candidates for snapping
       -> Client -- ^ The client to be moved
       -> Int -- ^ Border width
       -> Int -- ^ Snap distance
       -> Position -- ^ New position for client
       -> Position -- ^ Either a position with resistance or just the new position
doSnap clients client bw distance pos = final_position' closest_borders_dir
-- doSnap clients client bw distance pos = final_position closest_borders
    where
    dirs = directions (client ^. geometry . position) pos
    closest_borders = map (cropBorder bw) $ closestBorders clients client
    closest_borders_dir = closestBordersDirection closest_borders dirs
    px = pos ^. x
    py = pos ^. y

    safeHead p []    = p
    safeHead _ (p:_) = p

    cw = fi $ client ^. geometry . dimension . width
    ch = fi $ client ^. geometry . dimension . height

    border :: (Direction, Int) -> Int
    border (e, _)
        | e == North || e == West = px
        | e == South              = px + ch
        | e == East               = px + cw

    final_position' :: (Maybe (Direction, Int), Maybe (Direction, Int)) -> Position
    final_position' (ex, ey) = Position
       (fromMaybe px $ ex >>= (\eb -> snap distance (border eb) eb))
       (fromMaybe py $ ey >>= (\eb -> snap distance (border eb) eb))

    -- final_position :: [(Direction, Int)] -> Position
    -- final_position es = Position (safeHead px . fst $ candidates es)
    --                              (safeHead py . snd $ candidates es)

    -- candidates :: [(Direction, Int)] -> ([Int], [Int])
    -- candidates es = (positions ([], []) es)


    -- positions :: ([Maybe Int], [Maybe Int]) -> [(Direction, Int)] -> ([Int], [Int])
    -- positions (xs, ys) [] = (L.sort $ catMaybes xs, L.sort $ catMaybes ys)
    -- positions (xs, ys) ((e, b):es)
    --     | e == North || e == South =
    --         positions (xs, snap client distance py (e, b) : ys) es
    --     | otherwise =
    --         positions (snap client distance px (e, b) : xs, ys) es


    -- Position
    --     (fromMaybe px $ ex >>= snap client distance px)
    --     (fromMaybe py $ ey >>= snap client distance py)


type Border = Int
type Distance = Int
type BorderWidth = Int

doSnap2 :: BorderWidth
        -> Distance
        -> [Geometry]
        -> Geometry
        -> Position
        -> (Maybe Border, Maybe Border)
-- doSnap2 bw d gs ag p = closest_borders $ directions (ag ^. position) p
doSnap2 bw distance geometries g p = closest_borders $ directionsFromRelative p
    where
    -- closest_border = closestBorder' d gs ag
    closest_border edge = closestBorder'' distance edge geometries g
    closest_borders (ex, ey) = (ex >>= closest_border, ey >>= closest_border)


-- findSnappedBorder :: [Geometry]
--                   -> Geometry
--                   -> (Maybe Direction, Maybe Direction)
--                   -> (Maybe Border, Maybe Border)
-- findSnappedBorder gs g (mdx, mdy) = (mdx >>= find borders, mdy >>= find borders)
--     where
--     borders = concatMap (compareBorders g) gs
--     find :: [(Direction, Border)] -> Direction -> Maybe Border
--     find ((d,b):dbs) dir = if d == dir then Just b else find dbs dir
--     find _ _             = Nothing


snapBorders :: Geometry
            -> [Geometry]
            -> (Maybe (Direction, Border), Maybe (Direction, Border))
snapBorders g = result . concatMap (compareBorders g)
    where
    result :: [(Direction, Border)]
           -> (Maybe (Direction, Border), Maybe (Direction, Border))
    result (db1:db2:_) = mktuple db1 $ mktuple db2 (Nothing, Nothing)
    result (db:_)      = mktuple db (Nothing, Nothing)
    result _           = (Nothing, Nothing)

    mktuple :: (Direction, Border)
            -> (Maybe (Direction, Border), Maybe (Direction, Border))
            -> (Maybe (Direction, Border), Maybe (Direction, Border))
    mktuple (d, b) (mb1, mb2) = ( if d == East  || d == West  then Just (d, b) else mb1
                                , if d == North || d == South then Just (d, b) else mb2
                                )

    -- isOverlapping :: Geometry -> Geometry -> Bool
    -- isOverlapping g1 g2 = foldr1 (||) $ map (\d -> hasOverlap d g1 g2)
    --                                         [North, South, East, West]


compareBorders :: Geometry -> Geometry -> [(Direction, Border)]
compareBorders g1 g2 = catMaybes $ map (cmp g1 g2)
    [ (North, (north, south))
    , (South, (south, north))
    , (East,  (east, west))
    , (West,  (west, east))
    ]
    where
    cmp :: Geometry
        -> Geometry
        -> (Direction, (Geometry -> Border, Geometry -> Border))
        -> Maybe (Direction, Border)
    cmp g1 g2 (d, (f1, f2))
        | f1 g1 == f2 g2 && hasOverlap d g1 g2 = Just (d, f1 g1)
        | otherwise                            = Nothing

    -- compare b' b
    --     | b' == b   = Just b
    --     | otherwise = Nothing


    -- mpos :: (Maybe Border, Maybe Border) -> Maybe Position
    -- mpos (Nothing, Nothing) = Nothing
    -- mpos (Just x', Nothing) = Just $ Position       x'  (p ^. y)
    -- mpos (Nothing, Just y') = Just $ Position (p ^. x)        y'
    -- mpos (Just x', Just y') = Just $ Position       x'        y'

-- directions :: Position -> Position -> (Maybe Direction, Maybe Direction)

    -- og ^= other geometry; ag ^= active geometry

-- data Axis = X | Y
--     deriving Typeable

-- data Border = Border
--     { axis :: Axis
--     , position :: Int
--     }
--     deriving Typeable

class Delta a where
    delta :: a -> a -> a

instance Delta Position where
    delta (Position x1 y1) (Position x2 y2) = Position (x1 - x2) (y1 - y2)

-- data Delta = Delta Int Int
--     deriving Typeable

closestBorder'' :: Distance
               -> Direction
               -> [Geometry]
               -> Geometry
               -> Maybe Border
-- closestBorders'' distance edge geometries g = nearest edge
--                            $ catMaybes
--                            $ filter distance_pred
--                            $ map (candidates edge g)
--                            $ filter (hasOverlap edge g)
--                            $ gs

closestBorder'' distance direction geometries g = (correction direction g) <$>
    ( closest direction
    . filter (within distance direction $ border direction g)
    . borders (oppositeDirection direction)
    . filter (hasOverlap direction g)
    $ g `L.delete` geometries)

    -- where distance_pred = fromMaybe False . fmap (within d edge $ border edge g)


-- closestBorder' :: Distance -> Direction -> [Geometry] -> Geometry -> Maybe Border
-- closestBorder' d gs g edge = nearest edge
--                            $ catMaybes
--                            $ filter distance_pred
--                            $ map (candidates edge g)
--                            $ filter (hasOverlap edge g)
--                            $ gs
--     where distance_pred = fromMaybe False . fmap (within d edge $ border edge g)


correction :: Direction -> Geometry -> Border -> Border
correction d g b
    | d == North || d == West = b
    | d == South              = b - fi (g ^. dimension . height)
    | d == East               = b - fi (g ^. dimension . width)


borders :: Direction -> [Geometry] -> [Border]
borders _     [] = []
borders e (g:gs) = border e g : borders e gs


-- candidates :: Direction -> Geometry -> Geometry -> Maybe Border
-- candidates e ag og
--     | e == North && south og < north ag = Just $ south og
--     | e == South && north og > south ag = Just $ north og
--     | e == West  &&  east og < west ag  = Just $ east og
--     | e == East  &&  west og > east ag  = Just $ west og
--     | otherwise                         = Nothing


hasOverlap :: Direction -> Geometry -> Geometry -> Bool
hasOverlap e ag og
    | e == North || e == South = (west ag >= west og && west ag <= east og)
                              || (east ag <= east og && east ag >= west og)
    | otherwise =                (north ag >= north og && north ag <= south og)
                              || (south ag <= south og && south ag >= north og)


within :: Distance -> Direction -> Border -> Border -> Bool
within distance e ab ob
    | e == North || e == West = ab - ob >= 0 && ab - ob < distance
    | e == South || e == East = ob - ab >= 0 && ob - ab < distance
    | otherwise  = False


closest :: Direction -> [Border] -> Maybe Border
closest e [] = Nothing
closest e bs
    | e == North || e == West = Just $ maximum bs
    | e == South || e == East = Just $ minimum bs
    | otherwise               = Nothing


border :: Direction -> Geometry -> Int
border e g
    | e == North = north g
    | e == South = south g
    | e == East  = east g
    | e == West  = west g


north, south, east, west :: Geometry -> Int
north g = g ^. position . y
south g = g ^. position . y + fi (g ^. dimension . height)
east  g = g ^. position . x + fi (g ^. dimension . width)
west  g = g ^. position . x


{-
moveResist :: PointerMotion -> MotionNotifyEvent -> Z PointerStack ()
moveResist (M ppos) e = do
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
        -- TODO
        -- distance <- asks (resistDistance)
        let distance = 30
        bw <- askL $ config . borderWidth

        let closest_borders = closestBordersDirection
                                  (map (cropBorder $ fi bw)
                                       (closestBorders clients cclient))
                                  (directions (cclient ^. geometry . position)
                                              (Position px py))

            cx = fromMaybe px (fst closest_borders >>= resist cclient distance px)
            cy = fromMaybe py (snd closest_borders >>= resist cclient distance py)

        sendMessage_ (UpdateClient window $ updateXY cx cy)
        W.configure window $ [(ConfigWindowX, fi cx), (ConfigWindowY, fi cy)]
-}



handleMotionNotify :: MotionNotifyEvent -> Z PointerStack ()
handleMotionNotify e = get >>= handle
    where
    handle :: Maybe PointerMotion -> Z PointerStack ()
    handle Nothing              = return ()
    handle (Just (M _ _))         = moveMotionNotify e
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
