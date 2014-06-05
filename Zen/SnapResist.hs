{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             LambdaCase,
             TupleSections #-}

module SnapResist (moveSnapResist) where

import Data.Word
import Data.Maybe (listToMaybe, isJust, fromJust, catMaybes, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import qualified Data.List as L
import Data.Typeable
import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***), first, second)
import Control.Applicative ((<*>), (<$>), (<|>))
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
    let proximity = (100 :: Distance)
    -- snap_mod <- askL snapMod
    let snap_mod = KeyButMaskShift

    -- let snap_borders = join (***) (>>= do_snap_user snap_mod)
    --                  $ snapBorders proximity cgeometries cgeometry r_rel_pos

        -- resist_borders = join (***) (fmap adjust_border)
        --                $ resistBorders cgeometry cgeometries

    -- let bx = fst snap_borders <|> fst resist_borders >>= checkpos >>= 
    --     by = snd snap_borders <|> snd resist_borders

    let erx = e_rel_pos ^. x
    let ery = e_rel_pos ^. y
    let rrx = r_rel_pos ^. x
    let rry = r_rel_pos ^. y


    let cpos = cgeometry ^. position
    let npos = cgeometry ^. position + r_rel_pos
        ax = npos ^. x
        ay = npos ^. y

    let ax' = abs_pos ^. x
        ay' = abs_pos ^. y

    -- let mx = checkpos (fst move_directions) (fst snap_borders) (fst resist_borders)
    -- let my = checkpos (snd move_directions) (snd snap_borders) (snd resist_borders)

    -- let snap_position = Position (fromMaybe ax $ fst snap_borders)
    --                              (fromMaybe ay $ snd snap_borders)

    -- let resist_position = checkPosition cgeometry abs_pos

    let adjacent_borders = adjacentBorders cgeometry cgeometries
    -- let resist_position = checkPosition cgeometry snap_position adjacent_borders
    -- let resist_position = checkPosition cgeometry (Position ax' ay') proximity (erx, ery) adjacent_borders
    let resist_position = checkPosition cgeometry (Position ax' ay') adjacent_borders
    -- let resist_position = checkPosition cgeometry snap_position adjacent_borders

    -- let merger f s r = if isJust (f s) then f s else f r

    -- let mx = merger fst snap_borders resist_position
    -- let my = merger snd snap_borders resist_position

    -- let mx = (fst snap_borders) <|> (fst resist_position)
    -- let my = (snd snap_borders) <|> (snd resist_position)

    -- let mx = fst resist_position
    -- let my = snd resist_position

    -- let sx = fst snap_borders
    -- let sy = snd snap_borders

    -- let x_pred (d,_) = d == East  || d == West
    -- let y_pred (d,_) = d == North || d == South

    {-
    let sxs = filter x_pred adjacent_borders
    let sys = filter y_pred adjacent_borders

    let checkborder Nothing    (d,b) = Just (d,b)
        checkborder (Just dir) (d,b) = if d == dir then Just (d,b) else Nothing

    let sx = fmap snd $ listToMaybe $ if keypress snap_mod
        then sxs
        else catMaybes $ map (checkborder $ fst move_directions) sxs

    let sy = fmap snd $ listToMaybe $ if keypress snap_mod
        then sys
        else catMaybes $ map (checkborder $ snd move_directions) sys
    -}


    -- let sy = if keypress snap_mod
    --     then listToMaybe $ map snd $ filter y_pred adjacent_borders
    --     else Nothing


    -- let stick_x = fmap (unstick proximity erx) $ fst resist_position -- >>= stick proximity erx
    -- let stick_y = fmap (unstick proximity ery) $ snd resist_position -- >>= stick proximity ery

    -- let stick_x = fmap snd . listToMaybe . filter x_pred . catMaybes
    --             . stick adjacent_borders erx $ ax' -- (cpos ^. x)
    -- let stick_y = fmap snd . listToMaybe . filter y_pred . catMaybes
    --             . stick adjacent_borders ery $ ay' -- (cpos ^. y)

    -- let px = fromMaybe ax $  stick_x
    -- let py = fromMaybe ay $  stick_y

    -- let px = fromMaybe ax $ (fst snap_borders <|> stick_x)
    -- let py = fromMaybe ay $ (snd snap_borders <|> stick_y)
    -- let px = fromMaybe ax $ (sx <|> mx) >>= checkdelta proximity (e_rel_pos ^. x)
    -- let py = fromMaybe ay $ (sy <|> my) >>= checkdelta proximity (e_rel_pos ^. y)

    -- let px = fromMaybe ax xxx
    -- let py = fromMaybe ay yyy

    let nearest_borders = closestBorders cgeometry $ catMaybes $ nearestBorders proximity cgeometry cgeometries


    let nx (mx,_) = mx
    let ny (_,my) = my
    let nxlst (mx,_) = catMaybes [mx]
    let nylst (_,my) = catMaybes [my]

    -- let nx (_,_,e,w) = closest cgeometry e w
    -- let ny (n,s,_,_) = closest cgeometry n s
    -- let nxlst (_,_,e,w) = catMaybes [nx nearest_borders]
    -- let nylst (n,s,_,_) = catMaybes [ny nearest_borders]

            -- | d == East  && b  - b'  > 0 && b - b'  < proximity = Just b

            -- | (abs (b' - b) > 0 && abs (b' - b) < proximity) = Just b
            -- | b == b' = Just b

    -- let sx = if keypress snap_mod
    --     then snapBorder (fst move_directions) cgeometry (nxlst nearest_borders)

    let nxlst' = nxlst nearest_borders
    let snap_border_x = nx nearest_borders >>= snapBorder cgeometry (fst move_directions) -- nxlst'
    let sticky_x = nx nearest_borders >>= stickyBorder cgeometry
    let stick_x = fmap (finishBorder cgeometry)
                       (snap_border_x <|> sticky_x
                          >>= unstickBorder cgeometry proximity ax')

    let nylst' = nylst nearest_borders
    let snap_border_y = ny nearest_borders >>= snapBorder cgeometry (snd move_directions) -- nylst'
    let sticky_y = ny nearest_borders >>= stickyBorder cgeometry
    let stick_y = fmap (finishBorder cgeometry)
                       (snap_border_y <|> sticky_y
                          >>= unstickBorder cgeometry proximity ay')

    let resist_x = nx nearest_borders >>= useBorder proximity cgeometry ax'
    let resist_y = ny nearest_borders >>= useBorder proximity cgeometry ay'

    toLog $ ("border East cgeometry: " ++) . show $ border East cgeometry
    toLog $ ("border South cgeometry: " ++) . show $ border South cgeometry
    toLog $ ("nxlst': " ++) . show $ nxlst'
    toLog $ ("nylst': " ++) . show $ nylst'
    toLog $ ("snap_border_x: " ++) . show $ snap_border_x
    toLog $ ("snap_border_y: " ++) . show $ snap_border_y
    toLog $ ("sticky_x: " ++) . show $ sticky_x
    toLog $ ("stick_x: " ++) . show $ stick_x
    toLog $ ("sticky_y: " ++) . show $ sticky_y
    toLog $ ("stick_y: " ++) . show $ stick_y

    toLog $ ("resist_x: " ++) . show $ resist_x
    toLog $ ("resist_y: " ++) . show $ resist_y

    let px = fromMaybe ax' $ if keypress snap_mod
        then stick_x
        else resist_x

    let py = fromMaybe ay' $ if keypress snap_mod
        then stick_y
        else resist_y


    -- let px = fromMaybe ax' $ (nx nearest_borders)
    --                          >>= useBorder proximity cgeometry ax'
    -- let py = fromMaybe ay' $ (ny nearest_borders)
    --                          >>= useBorder proximity cgeometry ay'

    -- toLog $ ("snapBorder x: " ++) . show $ snapBorder (if keypress snap_mod then fst move_directions else Nothing)
    --                                                   cgeometry
    --                                                   (nxlst nearest_borders)
    -- toLog $ ("snapBorder y: " ++) . show $ snapBorder (if keypress snap_mod then snd move_directions else Nothing)
    --                                                   cgeometry
    --                                                   (nylst nearest_borders)

    -- let px' = if keypress snap_mod
    --     then case fst move_directions of
    --             Just dir -> Just $ nx nearest_borders
    --             _ -> Nothing
    --     else Nothing


    -- toLog $ "xxx: " ++ show xxx
    -- toLog $ "yyy: " ++ show yyy
    -- toLog $ ("adjacentBorders: " ++) . show $ adjacentBorders cgeometry cgeometries
    -- toLog $ ("makePosition: " ++) . show $ makePosition cgeometry $ adjacentBorders cgeometry cgeometries

    toLog $ ("move_directions: " ++) . show $ move_directions
    toLog $ ("cgeometry: " ++) . show $ cgeometry
    -- toLog $ ("npos: " ++) . show $ npos
    -- toLog $ ("cpos: " ++) . show $ cpos
    -- toLog $ ("cpos - root_pos: " ++) . show $ cpos - root_pos
    -- toLog $ ("cpos - event_pos: " ++) . show $ cpos - event_pos
    -- toLog $ ("abs_pos: " ++) . show $ abs_pos
    toLog $ ("rpos: " ++) . show $ rpos
    toLog $ ("epos: " ++) . show $ epos
    toLog $ ("root_pos: " ++) . show $ root_pos
    toLog $ ("event_pos: " ++) . show $ event_pos
    toLog $ ("r_rel_pos: " ++) . show $ r_rel_pos
    toLog $ ("e_rel_pos: " ++) . show $ e_rel_pos
    -- toLog $ ("snap_borders: " ++) . show $ snap_borders
    -- toLog $ ("resist_position: " ++) . show $ resist_position
    -- toLog $ ("adjacent_borders: " ++) . show $ adjacent_borders
    toLog $ "ax : " ++ show ax  ++ ", ay : " ++ show ay
    toLog $ "ax': " ++ show ax' ++ ", ay': " ++ show ay'
    toLog $ "px : " ++ show px  ++ ", py : " ++ show py

    -- toLog $ ("stick x: " ++) . show $ stick_x
    -- toLog $ ("stick y: " ++) . show $ stick_y

    toLog $ ("nearestBorders: " ++) . show $ nearest_borders

    -- toLog $ "resistBorders: " ++ show resist_borders
    -- toLog $ "yyy: " ++ show yyy

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

    root_pos = Position (fi root_x) (fi root_y)
    event_pos = Position (fi event_x) (fi event_y)
    abs_pos = Position (fi root_x) (fi root_y) - epos
    r_rel_pos = Position (fi root_x) (fi root_y) - rpos
    e_rel_pos = Position (fi event_x) (fi event_y) - epos

    -- let delta = abs $ epos - Position (fi $ event_x) (fi $ event_y)
    -- rdelta = delta rpos $ Position root_x root_y
    -- edelta = delta epos $ Position (fi event_x) (fi event_y)

    move_directions = directions r_rel_pos

    keypress k = k `elem` state_MotionNotifyEvent e
    do_snap_user k b = if keypress k then Just b else Nothing
    -- do_snap_proximity d p b = if d > p then Just b else Nothing

    adjust_border (d, b) = (d, adjustBorder d cgeometry b)

    -- mx = checkpos (fst move_directions)
    -- my = checkpos (snd move_directions)

    -- proximity = 50
    -- snap_borders = snapBorders' cgeometry cgeometries proximity move_directions
    -- adjacent_borders = adjacentBorders cgeometry cgeometries

    -- resist_borders = resistBorders'' move_directions $ adjacent_borders
    -- resist_borders = resistBorders''' cgeometry move_directions $ adjacent_borders

    -- xxx = fst resist_borders >>= checkdelta' proximity (edelta ^. x)
    -- yyy = snd resist_borders >>= checkdelta' proximity (edelta ^. y)

    -- xxx = fst resist_borders >>= checkdelta' proximity (edelta ^. x)
    -- yyy = snd resist_borders >>= checkdelta' proximity (edelta ^. y)
    -- xxx = fst snap_borders <|> fst resist_borders >>= checkdelta' proximity (edelta ^. x)
    -- yyy = snd snap_borders <|> snd resist_borders >>= checkdelta' proximity (edelta ^. y)

    checkpos :: Maybe Direction -> Maybe Border -> Maybe (Direction, Border) -> Maybe Border
    checkpos _    def Nothing   = def
    checkpos mdir _   (Just db) = checkdir db mdir

    checkdir :: (Direction, Border) -> Maybe Direction -> Maybe Border
    checkdir (_, b) Nothing    = Just b
    checkdir (d, b) (Just dir) = if d == dir then Just b else Nothing

    -- checkdelta :: Border -> Distance -> Delta -> Border -> Border
    -- checkdelta def p d b = if d > p then def else b

    checkdelta :: Distance -> Delta -> Border -> Maybe Border
    checkdelta p d b = if abs d > p then Nothing else Just b

    -- checkdelta p d b
    --     | abs d > p = Just (b + d)
    --     | abs d > 0 = Just b
    --     | otherwise = Just b

        -- if abs d > 0 then Just (b + d) else Just b

    -- delta = (0, 101); prox = 100
    -- when 101 > 100
    -- checkdelta :: Distance
    --            -> (Delta, Delta)
    --            -> (Maybe Border, Maybe Border)
    --            -> (Maybe Border, Maybe Border)
    -- checkdelta p d b = if d > p then Nothing else Just b


    -- checkdelta def p d b = if d > p then def else if d == p then b - p else b

    -- checkdelta' :: Distance -> Delta -> Border -> Maybe Border
    -- checkdelta' p d b = if p > d then Just b else Nothing


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
    let r_rel_pos = Position (fi root_x) (fi root_y) - rpos

    let bw = 6
        dist = 50
        snap_key = KeyButMaskShift
        want_snap = snap_key `elem` state_MotionNotifyEvent e
        cgeometry = client ^. geometry
        cgeometries = cgeometry `L.delete` map (^. geometry) clients

    toLog . ("client: " ++) $ show client
    toLog . ("doSnap2: " ++) . show $ doSnap2 bw dist cgeometries cgeometry r_rel_pos

    let dirs = directions r_rel_pos
    toLog $ ("directions: " ++) . show $ dirs

    let applyCorrection (d, b) = (d, adjustBorder d cgeometry b)
    let snapped_borders = first (fmap applyCorrection)
                        $ second (fmap applyCorrection)
                        $ snapBorders cgeometry cgeometries

    toLog . ("snapBorders: " ++) . show $ snapped_borders

    let check_snap b = if want_snap then Just b else Nothing
    let snap_pos = join (***) (>>= check_snap)
                 $ doSnap2 bw dist cgeometries cgeometry r_rel_pos

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
    toLog $ ("r_rel_pos: " ++) . show $ r_rel_pos
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


-- snapBorders' :: Geometry
--             -> [Geometry]
--             -> Distance
--             -> (Maybe Direction, Maybe Direction)
--             -> (Maybe Border, Maybe Border)
-- snapBorders' cg cgs distance = closest_borders
--     where
--     closest_border edge = closestBorder distance edge cgs cg
--     closest_borders (ex, ey) = (ex >>= closest_border, ey >>= closest_border)

-- snapBorders :: 
--            Distance
--         -> [Geometry]
--         -> Geometry
--         -> Position
--         -> (Maybe Border, Maybe Border)
-- snapBorders distance geometries g p = closest_borders $ directions p
--     where
--     closest_border edge = closestBorder distance edge geometries g
--     closest_borders (ex, ey) = (ex >>= closest_border, ey >>= closest_border)


{-
resistBorders' :: Geometry
               -> [Geometry]
               -> (Maybe Direction, Maybe Direction)
               -> (Maybe Border, Maybe Border)
resistBorders' g = undefined
-}


resistBorders''' :: Geometry
                 -> (Maybe Direction, Maybe Direction)
                 -> [(Direction, Border)]
                 -> (Maybe Border, Maybe Border)
resistBorders''' g (dx, dy) dbl = (findBorderX dbl dx, findBorderY dbl dy)
    where
    -- findBorderX ((d,b):dbs) dir
    --     -- | d == dir && (d == East || d == West) = if border d g == b then Just b else findBorderX dbs
    --     | d == dir = if border d g == b then Just b else findBorderX dbs dir
    --     | otherwise = findBorderX dbs dir
    -- findBorderX _           _ = Nothing

    -- findBorderY ((d,b):dbs) dir
    --     -- | d == North || d == South = if border d g == b then Just b else findBorderY dbs
    --     | d == dir = if border d g == b then Just b else findBorderY dbs dir
    --     | otherwise = findBorderY dbs dir
    -- findBorderY _           _ = Nothing

    check (d,b) f = if border d g == b then Just b else f

    -- findBorder ((d,b):dbs) dir
    --     | d == dir = check (d,b) $ findBorder dbs dir
    --     | otherwise = findBorder dbs dir
    -- findBorder _ _ = Nothing


    findBorderY ((d,b):dbs) (Just dir)
        | d == dir = check (d,b) $ findBorderY dbs $ Just dir
        | otherwise = findBorderY dbs $ Just dir
    findBorderY ((d,b):dbs) Nothing
        | d == North || d == South = check (d,b) $ findBorderY dbs Nothing
        | otherwise = findBorderY dbs Nothing
    findBorderY _ _ = Nothing


    findBorderX ((d,b):dbs) (Just dir)
        | d == dir = check (d,b) $ findBorderX dbs $ Just dir -- if border d g == b then Just b else findBorder dbs dir
        | otherwise = findBorderX dbs $ Just dir
    findBorderX ((d,b):dbs) Nothing
        | d == East || d == West = check (d,b) $ findBorderX dbs Nothing
        | otherwise = findBorderX dbs Nothing
    findBorderX _ _ = Nothing


        -- = if  == dir then Just b else findBorder dbs dir


resistBorders'' :: (Maybe Direction, Maybe Direction)
                -> [(Direction, Border)]
                -> (Maybe Border, Maybe Border)
resistBorders'' (dx, dy) dbl = (dx >>= findBorder dbl, dy >>= findBorder dbl)
    where
    findBorder ((d,b):dbs) dir = if d == dir then Just b else findBorder dbs dir
    findBorder _           _   = Nothing


{-
makePosition' :: Geometry
              -> (Maybe Direction, Maybe Direction)
              -> [(Direction, Border)]
              -> (Maybe Int, Maybe Int)
makePosition' g (dx, dy) dbs = (fmap pos mxpos, fmap pos mypos)
    where
    mxpos = join . listToMaybe . map (checkdir dx) . filter xpred $ dbs
    mypos = join . listToMaybe . map (checkdir dy) . filter ypred $ dbs

    xpred (d,_) = d == East  || d == West
    ypred (d,_) = d == North || d == South

    checkdir Nothing    (d,b) = Just (d,b)
    checkdir (Just dir) (d,b) = if d == dir then Just (d,b) else Nothing

    pos (d, b) | d == South || d == East = border (opposite d) g
               | otherwise = b
-}


unstick :: Distance -> Delta -> Int -> Int
unstick proximity delta b = if abs delta < proximity then b else b + delta

checkPosition :: Geometry
              -> Position
              -> [(Direction, Border)]
              -> (Maybe Int, Maybe Int)
checkPosition g (Position npx npy) dbs = (xs, ys)
    where
    xs = fmap pos . listToMaybe . filter (pred (East , West )) $ dbs
    ys = fmap pos . listToMaybe . filter (pred (North, South)) $ dbs

    pred (d1,d2) (d,b) = (d == d1 || d == d2) && check (d,b)

    pos (d, b) | d == South || d == East = border (opposite d) g
               | otherwise = b

    check (d,b)
        | d == North = npy < b
        | d == South = npy > b
        | d == East  = npx > b
        | d == West  = npx < b


{-
checkPosition :: Geometry
              -> Position
              -> [(Direction, Border)]
              -> (Maybe Int, Maybe Int)
checkPosition g (Position npx npy) dbs = (xs, ys)
    where
    xs = fmap pos . listToMaybe . filter xpred $ dbs
    ys = fmap pos . listToMaybe . filter ypred $ dbs

    xpred (d,b) = (d == East  || d == West ) && check (d,b)
    ypred (d,b) = (d == North || d == South) && check (d,b)

    pos (d, b) | d == South || d == East = border (opposite d) g
               | otherwise = b

    check (d,b)
        | d == North = npy < b
        | d == South = npy > b
        | d == East  = npx > b
        | d == West  = npx < b
-}


makePosition :: Geometry -> [(Direction, Border)] -> (Maybe Int, Maybe Int)
makePosition g dbs = (listToMaybe xs, listToMaybe ys)
    where xs = map pos $ filter (\(d,_) -> d == East  || d == West ) dbs
          ys = map pos $ filter (\(d,_) -> d == North || d == South) dbs
          pos (d, b) | d == South || d == East = border (opposite d) g
                     | otherwise = b


adjacentBorders :: Geometry
              -> [Geometry]
              -> [(Direction, Border)]
adjacentBorders g = concatMap (compareBorders g)


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


finishBorder :: Geometry -> (Direction, Border) -> Border
finishBorder g (d,b) = adjustBorder d g b

-- unstickBorder :: Geometry -> Distance -> Border -> Border -> Maybe Border
unstickBorder :: Geometry -> Distance -> Border -> (Direction, Border) -> Maybe (Direction, Border)
unstickBorder g p b' (d,b) -- = if abs (b' - b) < p then Just b else Nothing
    | (d == North || d == West) && abs (b' - b) < p                                  = Just (d,b)
    |  d == South               && abs (b' - (b - fi (g ^. dimension . height))) < p = Just (d,b)
    |  d == East                && abs (b' - (b - fi (g ^. dimension . width))) < p  = Just (d,b)
    | otherwise                                                = Nothing
    -- | d == South && abs (b' - (b - border d g)) < p = Just (d,b)
    -- b - border g 

    -- | d == South && b' - bs < p = Just bs
    -- | d == East  && b' - be < p = Just be
    -- | otherwise                                        = Nothing
    -- where bs = b - fi (g ^. dimension . height)
    --       be = b - fi (g ^. dimension . width)



stickyBorder :: Geometry -> (Direction, Border) -> Maybe (Direction, Border)
stickyBorder g (d,b) = if border d g == b then Just (d,b) else Nothing
    -- where isSticky (d,b) = border d g == b


snapBorder :: Geometry
           -> Maybe Direction
           -> (Direction, Border)
           -> Maybe (Direction, Border)
snapBorder _ Nothing    _     = Nothing
snapBorder g (Just dir) (d,b) = if d == dir then Just (d,b) else Nothing


useBorder :: Distance
          -> Geometry
          -> Border
          -> (Direction, Border)
          -> Maybe Border
useBorder proximity cgeometry b' (d,b)
    | d == North && b  - b' > 0 && b  - b' < proximity = Just b
    | d == West  && b  - b' > 0 && b  - b' < proximity = Just b
    | d == South && b' - bs > 0 && b' - bs < proximity = Just bs
    | d == East  && b' - be > 0 && b' - be < proximity = Just be
    | otherwise                                        = Nothing
    where bs = b - fi (cgeometry ^. dimension . height)
          be = b - fi (cgeometry ^. dimension . width)


-- nearestBorders :: Distance -> Geometry -> [Geometry]
--                -> ( Maybe (Direction, Border), Maybe (Direction, Border)
--                   , Maybe (Direction, Border), Maybe (Direction, Border))
-- nearestBorders distance g gs = (nb, sb, eb, wb)
nearestBorders distance g gs = [nb, sb, eb, wb]
    where
    nb = fmap (North,) $ listTo maximum $ map (south) $ filter north_pred gs
    sb = fmap (South,) $ listTo minimum $ map (north) $ filter south_pred gs
    eb = fmap (East,)  $ listTo minimum $ map (west ) $ filter east_pred  gs
    wb = fmap (West,)  $ listTo maximum $ map (east ) $ filter west_pred  gs

    listTo f [] = Nothing
    listTo f ls = Just $ f ls

    north_pred g' = north g >= south g' && hasOverlap North g g'
    south_pred g' = south g <= north g' && hasOverlap South g g'
    east_pred  g' = east  g <= west  g' && hasOverlap East  g g'
    west_pred  g' = west  g >= east  g' && hasOverlap West  g g'


-- closest :: Geometry
--         -> Maybe (Direction, Border)
--         -> Maybe (Direction, Border)
--         -> Maybe (Direction, Border)
-- closest _ Nothing    Nothing   = Nothing
-- closest _ (Just db)  Nothing   = (Just db)
-- closest _ Nothing    (Just db) = (Just db)
-- closest g (Just (d1,b1)) (Just (d2,b2))
--     | d1 == North = Just $ if b1 - s < n - b2 then (d2,b2) else (d1,b1)
--     | d1 == East  = Just $ if w - b1 < b2 - e then (d2,b2) else (d1,b1)
--     | otherwise = error "closest :: Geometry -> Maybe (Direction, Border) -> Maybe (Direction, Border) -> Maybe (Direction, Border)"
--     where
--     n = north g
--     s = south g
--     e = east g
--     w = west g


closestBorders :: Geometry
               -> [(Direction, Border)]
               -> (Maybe (Direction, Border), Maybe (Direction, Border))
closestBorders g dbs = (mx, my)
    where
    mx = listToMaybe . L.sortBy cmp . filter xs $ dbs
    my = listToMaybe . L.sortBy cmp . filter ys $ dbs

    xs (d,b) = d == East  || d == West
    ys (d,b) = d == North || d == South

    cmp (d1,b1) (d2,b2)
        | abs (b1 - border (opposite d1) g) < abs (b2 - border (opposite d2) g) = LT
        | otherwise                                                             = GT


compareBorders :: Geometry -> Geometry -> [(Direction, Border)]
compareBorders g' g'' = catMaybes $ map (cmp g' g'')
    [ (North, (north, south))
    , (South, (south, north))
    , (East,  (east, west))
    , (West,  (west, east))
    ]
    where
    fuzz = 10
    cmp :: Geometry
        -> Geometry
        -> (Direction, (Geometry -> Border, Geometry -> Border))
        -> Maybe (Direction, Border)
    cmp g1 g2 (d, (f1, f2))
        -- ???
        -- | f1 g1 == f2 g2 && hasOverlap d g1 g2 = Just (d, f1 g1)
        | f1 g1 == f2 g2 = Just (d, f1 g1)
        -- | f2 g2 < (f1 g1 + fuzz) && f2 g2 > (f1 g1 - fuzz) = Just (d, f2 g2)
        | otherwise                            = Nothing


-- closestBorder :: Distance
--                -> Direction
--                -> [Geometry]
--                -> Geometry
--                -> Maybe Border
-- closestBorder distance direction geometries g = (adjustBorder direction g) <$>
--     ( closest direction
--     . filter (within distance direction $ border direction g)
--     . borders (opposite direction)
--     . filter (hasOverlap direction g)
--     $ g `L.delete` geometries)


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


-- closest :: Direction -> [Border] -> Maybe Border
-- closest _ [] = Nothing
-- closest e bs
--     | e == North || e == West = Just $ maximum bs
--     | e == South || e == East = Just $ minimum bs
--     | otherwise               = Nothing


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
