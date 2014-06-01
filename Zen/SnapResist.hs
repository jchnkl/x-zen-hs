{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             LambdaCase,
             TupleSections #-}

module SnapResist (moveSnapResist) where

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


-- moveMotionNotify :: MotionNotifyEvent -> Z PointerStack ()
-- moveMotionNotify e = get >>= \case
--     Just m -> do
--         let bw = 3
--         mclient <- join . fmap getClientReply <$> sendMessage (GetClient window)
--         clients <- maybe [] (map (addbw bw) . M.elems . getQueueReply) <$> sendMessage GetQueue
--         whenJustM_ mclient $ doMoveMotionNotify e m clients . addbw bw
--     _ -> return ()
--     where window = event_MotionNotifyEvent e
--           addbw bw = -- (geometry . position . x -~ bw)
--                    -- . (geometry . position . y -~ bw)
--                      (geometry . dimension . width +~ 2 * bw)
--                    . (geometry . dimension . height +~ 2 * bw)


-- data Delta a = Delta a

type Delta = Int
type Border = Int
type Distance = Int
-- type BorderWidth = Int

class DeltaClass a where
    delta :: a -> a -> a

instance DeltaClass Position where
    delta p1 p2 = abs (p2 - p1)




moveSnapResist e epos rpos client clients = do
    -- bwidth <- asksL (config . borderWidth) (2 *)
    -- proximity <- askL snapProximity
    let proximity = (50 :: Distance)
    -- snap_mod <- askL snapMod
    let snap_mod = KeyButMaskShift

    let snap_borders = join (***) (>>= do_snap_user snap_mod)
                     $ snapBorders proximity cgeometries cgeometry rel_pos

        resist_borders = join (***) (fmap adjust_border)
                       $ resistBorders cgeometry cgeometries

    -- let bx = fst snap_borders <|> fst resist_borders >>= checkpos >>= 
    --     by = snd snap_borders <|> snd resist_borders

    let ax = abs_pos ^. x
        ay = abs_pos ^. y

        -- px = fromMaybe ax $ mx (fst snap_borders >>= do_snap_proximity (edelta ^. x) proximity)
        --                        (fst resist_borders)

        -- py = fromMaybe ay $ my (snd snap_borders >>= do_snap_proximity (edelta ^. y) proximity)
        --                        (snd resist_borders)



    let mx = checkpos (fst move_directions) (fst snap_borders) (fst resist_borders)
    let my = checkpos (snd move_directions) (snd snap_borders) (snd resist_borders)

    let px = fromMaybe ax $ fmap (checkdelta ax proximity (edelta ^. x)) mx
    let py = fromMaybe ay $ fmap (checkdelta ay proximity (edelta ^. y)) my

    sendMessage_ $ ModifyClient window $ changePosition
                 $ Position px py

    W.configure window [(ConfigWindowX, fi px), (ConfigWindowY, fi py)]

    where
    window = event_MotionNotifyEvent e
    root_x = root_x_MotionNotifyEvent e
    root_y = root_y_MotionNotifyEvent e
    event_x = event_x_MotionNotifyEvent e
    event_y = event_y_MotionNotifyEvent e

    cgeometry = client ^. geometry
    cgeometries = cgeometry `L.delete` map (^. geometry) clients

    abs_pos = Position (fi root_x) (fi root_y) - epos
    rel_pos = Position (fi root_x) (fi root_y) - rpos

    -- let delta = abs $ epos - Position (fi $ event_x) (fi $ event_y)
    -- rdelta = delta rpos $ Position root_x root_y
    edelta = delta epos $ Position (fi event_x) (fi event_y)

    move_directions = directions rel_pos

    do_snap_user k b = if k `elem` state_MotionNotifyEvent e then Just b else Nothing
    -- do_snap_proximity d p b = if d > p then Just b else Nothing

    adjust_border (d, b) = (d, adjustBorder d cgeometry b)

    -- mx = checkpos (fst move_directions)
    -- my = checkpos (snd move_directions)

    checkpos :: Maybe Direction -> Maybe Border -> Maybe (Direction, Border) -> Maybe Border
    checkpos _    def Nothing   = def
    checkpos mdir _   (Just db) = checkdir db mdir

    checkdir :: (Direction, Border) -> Maybe Direction -> Maybe Border
    checkdir (_, b) Nothing    = Just b
    checkdir (d, b) (Just dir) = if d == dir then Just b else Nothing

    checkdelta :: Border -> Distance -> Delta -> Border -> Border
    checkdelta def p d b = if d > p then def else b


    -- checkdir :: (Direction, Border) -> Direction -> Maybe Border
    -- checkdir (d, b) dir = if d == dir then Just b else Nothing

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



    changePosition :: Position -> Client -> Client
    changePosition p = geometry . position .~ p

{-
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

    let dirs = directions rel_pos
    toLog $ ("directions: " ++) . show $ dirs

    let applyCorrection (d, b) = (d, adjustBorder d cgeometry b)
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

    -- updatePosition :: Maybe Int -> Maybe Int -> Client -> Client
    -- updatePosition (Just x')   Nothing = geometry . position . x .~ x'
    -- updatePosition Nothing   (Just y') = geometry . position . y .~ y'
    -- updatePosition (Just x') (Just y') = (geometry . position . x .~ x')
    --                                    . (geometry . position . y .~ y')
    -- updatePosition _     _             = id

    -- makeSnapPosition :: Maybe (Maybe Border, Maybe Border) -> Position
    -- makeSnapPosition (Just (Just x', Nothing)) = Position x' 0
    -- makeSnapPosition (Just (Nothing, Just y')) = Position 0  y'
    -- makeSnapPosition (Just (Just x', Just y')) = Position x' y'
    -- makeSnapPosition _                         = nullPosition

    -- modifyPosition :: Position -> Client -> Client
    -- modifyPosition (Position x' y') = (geometry . position . x +~ x')
    --                                 . (geometry . position . y +~ y')

    -- configValues :: Maybe Int -> Maybe Int -> [(ConfigWindow, Word32)]
    -- configValues (Just x')   Nothing = [(ConfigWindowX, fi x')]
    -- configValues Nothing   (Just y') = [(ConfigWindowY, fi y')]
    -- configValues (Just x') (Just y') = [(ConfigWindowX, fi x'), (ConfigWindowY, fi y')]
    -- configValues _         _         = []

    -- configValues :: Position -> Position -> [(ConfigWindow, Word32)]
    -- configValues abs_p rel_p = cx ++ cy
    --     where
    --     cx = if rel_p ^. x /= 0 then [(ConfigWindowX, fi $ abs_p ^. x)] else []
    --     cy = if rel_p ^. y /= 0 then [(ConfigWindowY, fi $ abs_p ^. y)] else []
    --     -- [(ConfigWindowX, fi $ p ^. x), (ConfigWindowY, fi $ p ^. y)]

    -- w = fromXid $ toXid (0 :: Word32) :: WindowId
    -- warp x' y' = do
    --     -- toLog $ "warp to " ++ show p
    --     connection $-> \c -> io $ warpPointer c
    --         $ MkWarpPointer w (getRoot c) 0 0 0 0 (fi x') (fi y')
-}

opposite :: Direction -> Direction
opposite = \case
    North -> South
    South -> North
    East  -> West
    West  -> East


directions :: Position -> (Maybe Direction, Maybe Direction)
directions p = (x_edge, y_edge)
    where
    x_edge
        | (p ^. x) > 0 = Just East
        | (p ^. x) < 0 = Just West
        | otherwise    = Nothing
    y_edge
        | (p ^. y) < 0 = Just North
        | (p ^. y) > 0 = Just South
        | otherwise    = Nothing


snapBorders :: 
           Distance
        -> [Geometry]
        -> Geometry
        -> Position
        -> (Maybe Border, Maybe Border)
snapBorders distance geometries g p = closest_borders $ directions p
    where
    closest_border edge = closestBorder distance edge geometries g
    closest_borders (ex, ey) = (ex >>= closest_border, ey >>= closest_border)


resistBorders :: Geometry
            -> [Geometry]
            -> (Maybe (Direction, Border), Maybe (Direction, Border))
resistBorders g = result . concatMap (compareBorders g)
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


compareBorders :: Geometry -> Geometry -> [(Direction, Border)]
compareBorders g' g'' = catMaybes $ map (cmp g' g'')
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


closestBorder :: Distance
               -> Direction
               -> [Geometry]
               -> Geometry
               -> Maybe Border
closestBorder distance direction geometries g = (adjustBorder direction g) <$>
    ( closest direction
    . filter (within distance direction $ border direction g)
    . borders (opposite direction)
    . filter (hasOverlap direction g)
    $ g `L.delete` geometries)


adjustBorder :: Direction -> Geometry -> Border -> Border
adjustBorder d g b
    | d == North || d == West = b
    | d == South              = b - fi (g ^. dimension . height)
    | d == East               = b - fi (g ^. dimension . width)
    | otherwise               = error "adjustBorder :: Direction -> Geometry -> Border -> Border"


borders :: Direction -> [Geometry] -> [Border]
borders _     [] = []
borders e (g:gs) = border e g : borders e gs


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
closest _ [] = Nothing
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
    | otherwise  = error "border :: Direction -> Geometry -> Int"


north, south, east, west :: Geometry -> Int
north g = g ^. position . y
south g = g ^. position . y + fi (g ^. dimension . height)
east  g = g ^. position . x + fi (g ^. dimension . width)
west  g = g ^. position . x
