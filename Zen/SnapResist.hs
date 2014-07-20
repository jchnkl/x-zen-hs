-- vim:sw=4:sts=4:ts=4

{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             LambdaCase,
             TupleSections #-}

module SnapResist (moveSnapResist, snapResistComponent) where

import Data.Ord (comparing)
import Data.Function (on)
import Data.Word
import Data.Maybe (maybeToList, listToMaybe, isJust, fromJust, catMaybes, fromMaybe)
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
import qualified Model as Model
import Keyboard (getCleanMask, extraModifier)
import Component


type Border = Int
type Distance = Int
-- type BorderWidth = Int


-- TODO
-- distinguish between
-- Border: Left, Right, Top, Bottom
-- Edge: North, South, East, West
-- Axis: X, Y
-- borderToAxis
-- edgeToAxis
-- borderToEdge
-- edgeToBorder


data PointerPosition = PointerPosition { root :: Position, event :: Position }
    deriving Typeable

type SnapResistStackT = StateT (Maybe PointerPosition) (Z IO)

snapResistComponent :: ControllerComponent
snapResistComponent = Component
    { componentId = "SnapResist"
    , componentData = Nothing
    , execComponent = execSnapResistComponent
    , onStartup = return . id
    , onShutdown = const $ return ()
    , someHandler = const $ map SomeHandler
                          [ EventHandler handleButtonPress
                          , EventHandler handleButtonRelease
                          , EventHandler handleMotionNotify
                          ]
    }


putPP :: Maybe PointerPosition -> SnapResistStackT ()
putPP = put


getPP :: SnapResistStackT (Maybe PointerPosition)
getPP = get


execSnapResistComponent :: SnapResistStackT a
                        -> Maybe PointerPosition
                        -> Z IO (Maybe PointerPosition)
execSnapResistComponent m pp = execStateT m pp


handleButtonPress :: ButtonPressEvent -> SnapResistStackT ()
handleButtonPress e = putPP . Just $ PointerPosition (Position root_x root_y)
                                                     (Position event_x event_y)
    where
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e


handleButtonRelease :: ButtonPressEvent -> SnapResistStackT ()
handleButtonRelease _ = putPP Nothing


handleMotionNotify :: MotionNotifyEvent -> SnapResistStackT ()
handleMotionNotify e = getPP >>= \mpp -> whenJustM_ mpp $ \pp@(PointerPosition rpos epos) -> do
    let bw = 3
    mclient <- Model.lookup window
    clients <- map (addbw bw) <$> Model.toList
    whenJustM_ mclient $ \client -> do
        lift $ moveSnapResist e rpos epos client clients
        putPP . Just $ pp { root = (Position (fi root_x) (fi root_y)) }
    where window = event_MotionNotifyEvent e
          root_x = root_x_MotionNotifyEvent e
          root_y = root_y_MotionNotifyEvent e
          addbw bw = (geometry . dimension .  width +~ 2 * bw)
                   . (geometry . dimension . height +~ 2 * bw)


moveSnapResist :: MonadIO m
               => MotionNotifyEvent
               -> Position --  root_{x,y}_ButtonPressEvent
               -> Position -- event_{x,y}_ButtonPressEvent
               -> Client
               -> [Client]
               -> Z m ()
moveSnapResist e rpos epos client clients = do
    -- bwidth <- asksL (config . borderWidth) (2 *)
    -- proximity <- askL snapProximity
    let proximity = (100 :: Distance)
    -- snap_mod <- askL snapMod
    let snap_mod = KeyButMaskShift

    let npos = cgeometry ^. position + r_rel_pos
        ax = npos ^. x
        ay = npos ^. y

    let ax' = abs_pos ^. x
        ay' = abs_pos ^. y

    let nearest_borders = closestBorders cgeometry
                        $ adjacentBorders cgeometry cgeometries

    let nx (mx,_) = mx
    let ny (_,my) = my

    let next_border_x = fmap (nextBorder cgeometry cgeometries (fst move_directions)) (ny nearest_borders)
    let next_border_y = fmap (nextBorder cgeometry cgeometries (snd move_directions)) (nx nearest_borders)
    toLog $ ("next_border_x: " ++) . show $ next_border_x
    toLog $ ("next_border_y: " ++) . show $ next_border_y

    let snap_border_x = nx nearest_borders >>= snapBorder cgeometry (fst move_directions) -- nxlst'
    let sticky_x = nx nearest_borders >>= stickyBorder cgeometry
    -- let stick_x = fmap (finishBorder cgeometry)
    --                    (snap_border_x <|> sticky_x
    --                       >>= unstickBorder cgeometry proximity ax')
    let stick_x = fmap (finishBorder cgeometry)
                       (unstickBorder cgeometry proximity ax' $
                       (concat . maybeToList $ next_border_x)
                       ++
                       (maybeToList snap_border_x)
                       ++
                       (maybeToList sticky_x)
                       )

    let snap_border_y = ny nearest_borders >>= snapBorder cgeometry (snd move_directions) -- nylst'
    let sticky_y = ny nearest_borders >>= stickyBorder cgeometry
    -- let stick_y = fmap (finishBorder cgeometry)
    --                    (snap_border_y <|> sticky_y
    --                       >>= unstickBorder cgeometry proximity ay')
    let stick_y = fmap (finishBorder cgeometry)
                       (unstickBorder cgeometry proximity ay' $
                       (concat . maybeToList $ next_border_y)
                       ++
                       (maybeToList snap_border_y)
                       ++
                       (maybeToList sticky_y)
                       )

    let resist_x = useBorder' proximity cgeometry ax' $
                   (concat . maybeToList $ next_border_x)
                   ++
                   (maybeToList $nx nearest_borders)

    let resist_y = useBorder' proximity cgeometry ay' $
                   (concat . maybeToList $ next_border_y)
                   ++
                   (maybeToList $ ny nearest_borders)

    let px = fromMaybe ax' $ if keypress snap_mod
        then stick_x
        else resist_x

    let py = fromMaybe ay' $ if keypress snap_mod
        then stick_y
        else resist_y

    toLog $ ("north cgeometry: " ++) . show $ north cgeometry
    toLog $ ("south cgeometry: " ++) . show $ south cgeometry
    toLog $ ("east  cgeometry: " ++) . show $ east cgeometry
    toLog $ ("west  cgeometry: " ++) . show $ west cgeometry
    toLog $ ("move_directions: " ++) . show $ move_directions
    toLog $ ("cgeometry: " ++) . show $ cgeometry
    toLog $ ("rpos: " ++) . show $ rpos
    toLog $ ("epos: " ++) . show $ epos
    toLog $ ("r_rel_pos: " ++) . show $ r_rel_pos
    toLog $ "ax : " ++ show ax  ++ ", ay : " ++ show ay
    toLog $ "ax': " ++ show ax' ++ ", ay': " ++ show ay'
    toLog $ "px : " ++ show px  ++ ", py : " ++ show py

    toLog $ ("adjacentBorders: " ++) . show $ nearest_borders

    Model.setPosition window $ Position px py

    W.configure window [(ConfigWindowX, fi px), (ConfigWindowY, fi py)]

    where
    window = event_MotionNotifyEvent e
    root_x = root_x_MotionNotifyEvent e
    root_y = root_y_MotionNotifyEvent e

    cgeometry = client ^. geometry
    cgeometries = cgeometry `L.delete` map (^. geometry) clients

    abs_pos = Position (fi root_x) (fi root_y) - epos
    r_rel_pos = Position (fi root_x) (fi root_y) - rpos

    move_directions = directions r_rel_pos

    keypress k = k `elem` state_MotionNotifyEvent e

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


finishBorder :: Geometry -> (Direction, Border) -> Border
finishBorder g (d,b) = adjustBorder d g b

-- snap policy
unstickBorder :: Geometry
              -> Distance
              -> Border
              -> [(Direction, Border)]
              -> Maybe (Direction, Border)
unstickBorder g p b' = listToMaybe . catMaybes . map unstickBorder'
    where
    unstickBorder' (d,b)
        | (d == North || d == West) && abs (b' - b)  < p = Just (d,b)
        |  d == South               && abs (b' - bs) < p = Just (d,b)
        |  d == East                && abs (b' - be) < p = Just (d,b)
        | otherwise                                      = Nothing
        where bs = b - fi (g ^. dimension . height)
              be = b - fi (g ^. dimension . width)


-- just checks whether two clients share a border (?)
stickyBorder :: Geometry -> (Direction, Border) -> Maybe (Direction, Border)
stickyBorder g (d,b) = if border d g == b then Just (d,b) else Nothing


-- checks whether a border (with on an edge) matches the current move direction
snapBorder :: Geometry
           -> Maybe Direction
           -> (Direction, Border)
           -> Maybe (Direction, Border)
snapBorder _ Nothing    _     = Nothing
snapBorder _ (Just dir) (d,b) = if d == dir then Just (d,b) else Nothing


-- resist policy
useBorder' :: Distance
          -> Geometry
          -> Border
          -> [(Direction, Border)]
          -> Maybe Border
useBorder' proximity g b' = listToMaybe . catMaybes . map useBorder''
    where
    useBorder'' (d,b)
        | d == North && b  - b' > 0 && b  - b' < proximity = Just b
        | d == West  && b  - b' > 0 && b  - b' < proximity = Just b
        | d == South && b' - bs > 0 && b' - bs < proximity = Just bs
        | d == East  && b' - be > 0 && b' - be < proximity = Just be
        | otherwise                                        = Nothing
        where bs = b - fi (g ^. dimension . height)
              be = b - fi (g ^. dimension . width)


listTo :: ([t] -> a) -> [t] -> Maybe a
listTo _ [] = Nothing
listTo f ls = Just $ f ls


adjacentBorders :: Geometry -> [Geometry] -> [(Direction, Border)]
adjacentBorders g gs = catMaybes [nb, sb, eb, wb]
    where
    nb = fmap (North,) . listTo maximum . map (border South) . filter north_pred $ gs
    sb = fmap (South,) . listTo minimum . map (border North) . filter south_pred $ gs
    eb = fmap (East,)  . listTo minimum . map (border West ) . filter east_pred  $ gs
    wb = fmap (West,)  . listTo maximum . map (border East ) . filter west_pred  $ gs

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

    xs (d,_) = d == East  || d == West
    ys (d,_) = d == North || d == South

    cmp (d1,b1) (d2,b2)
        | abs (b1 - border (opposite d1) g) < abs (b2 - border (opposite d2) g) = LT
        | otherwise                                                             = GT



nextBorder :: Geometry
            -> [Geometry]
            -> Maybe Direction
            -> (Direction, Border)
            -> [(Direction, Border)]

nextBorder g gs md (d,_)
    | d == North || d == South = result ew_diffs
    | otherwise                = result ns_diffs
    where
    result diff_f = map fst . sortByDir []
                  . filter ((>=0) . snd) . concatMap diff_f
                  . filter ((border d g ==) . border (opposite d))
                  $ gs
    ns_diffs g' = [ ((North, north g'), north g  - north g')
                  , ((South, south g'), south g' - south g )
                  ]
    ew_diffs g' = [ ((East, east g'), east g' - east g )
                  , ((West, west g'), west g  - west g')
                  ]

    sortByDir r [] = r
    sortByDir r (l@((dir,_),_):rest)
        | md == Just dir = sortByDir (l : r) rest
        | otherwise      = sortByDir (r ++ [l]) rest


adjustBorder :: Direction -> Geometry -> Border -> Border
adjustBorder d g b
    | d == North || d == West = b
    | d == South              = b - fi (g ^. dimension . height)
    | d == East               = b - fi (g ^. dimension . width)
    | otherwise               = error "adjustBorder :: Direction -> Geometry -> Border -> Border"


hasOverlap :: Direction -> Geometry -> Geometry -> Bool
hasOverlap e ag og
    | e == North || e == South = (west ag > west og && west ag < east og)
                              || (east ag < east og && east ag > west og)
                              || east ag == east og
                              || west ag == west og
    | otherwise                = (north ag > north og && north ag < south og)
                              || (south ag < south og && south ag > north og)
                              || north ag == north og
                              || south ag == south og


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
