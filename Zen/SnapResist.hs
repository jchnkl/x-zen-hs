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

    let adjacent_borders = adjacentBorders cgeometry cgeometries
    let resist_position = checkPosition cgeometry (Position ax' ay') adjacent_borders


    let nearest_borders = closestBorders cgeometry $ catMaybes $ nearestBorders proximity cgeometry cgeometries


    let nx (mx,_) = mx
    let ny (_,my) = my
    let nxlst (mx,_) = catMaybes [mx]
    let nylst (_,my) = catMaybes [my]

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


    toLog $ ("move_directions: " ++) . show $ move_directions
    toLog $ ("cgeometry: " ++) . show $ cgeometry
    toLog $ ("rpos: " ++) . show $ rpos
    toLog $ ("epos: " ++) . show $ epos
    toLog $ ("root_pos: " ++) . show $ root_pos
    toLog $ ("event_pos: " ++) . show $ event_pos
    toLog $ ("r_rel_pos: " ++) . show $ r_rel_pos
    toLog $ ("e_rel_pos: " ++) . show $ e_rel_pos
    toLog $ "ax : " ++ show ax  ++ ", ay : " ++ show ay
    toLog $ "ax': " ++ show ax' ++ ", ay': " ++ show ay'
    toLog $ "px : " ++ show px  ++ ", py : " ++ show py

    toLog $ ("nearestBorders: " ++) . show $ nearest_borders

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

    move_directions = directions r_rel_pos

    keypress k = k `elem` state_MotionNotifyEvent e
    do_snap_user k b = if keypress k then Just b else Nothing

    adjust_border (d, b) = (d, adjustBorder d cgeometry b)

    checkpos :: Maybe Direction -> Maybe Border -> Maybe (Direction, Border) -> Maybe Border
    checkpos _    def Nothing   = def
    checkpos mdir _   (Just db) = checkdir db mdir

    checkdir :: (Direction, Border) -> Maybe Direction -> Maybe Border
    checkdir (_, b) Nothing    = Just b
    checkdir (d, b) (Just dir) = if d == dir then Just b else Nothing

    checkdelta :: Distance -> Delta -> Border -> Maybe Border
    checkdelta p d b = if abs d > p then Nothing else Just b

    changePosition :: Position -> Client -> Client
    changePosition p = geometry . position .~ p


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


resistBorders''' :: Geometry
                 -> (Maybe Direction, Maybe Direction)
                 -> [(Direction, Border)]
                 -> (Maybe Border, Maybe Border)
resistBorders''' g (dx, dy) dbl = (findBorderX dbl dx, findBorderY dbl dy)
    where
    check (d,b) f = if border d g == b then Just b else f


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


resistBorders'' :: (Maybe Direction, Maybe Direction)
                -> [(Direction, Border)]
                -> (Maybe Border, Maybe Border)
resistBorders'' (dx, dy) dbl = (dx >>= findBorder dbl, dy >>= findBorder dbl)
    where
    findBorder ((d,b):dbs) dir = if d == dir then Just b else findBorder dbs dir
    findBorder _           _   = Nothing


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

unstickBorder :: Geometry -> Distance -> Border -> (Direction, Border) -> Maybe (Direction, Border)
unstickBorder g p b' (d,b)
    | (d == North || d == West) && abs (b' - b) < p                                  = Just (d,b)
    |  d == South               && abs (b' - (b - fi (g ^. dimension . height))) < p = Just (d,b)
    |  d == East                && abs (b' - (b - fi (g ^. dimension . width))) < p  = Just (d,b)
    | otherwise                                                = Nothing


stickyBorder :: Geometry -> (Direction, Border) -> Maybe (Direction, Border)
stickyBorder g (d,b) = if border d g == b then Just (d,b) else Nothing


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
