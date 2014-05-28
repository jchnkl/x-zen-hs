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
    -- maybe [] unGetClientsReply <$> sendMessage GetClients
    -- let w = fromXid $ toXid (0 :: Word32) :: WindowId
    -- (sendMessage (IsClient w) :: Z IO (Maybe CoreMessageReply)) >>= io . putStrLn . show

    mapM_ (grabButtons bm buttons)
        =<< maybe [] (map (^. xid) . getClientsReply) <$> sendMessage GetClients

    -- fromMaybe [] <$> (sendMessage GetClients >>= waitForMessageReply)
    -- fromMaybe [] <$> (sendMessage GetClients)
    --         >>= mapM_ (grabButtons bm buttons)
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


-- doMoveResist :: ButtonPressEvent -> Z PointerStack ()
-- doMoveResist e = do
--     clients <- maybe [] getClientsReply <$> sendMessage GetClients
--     whenJustM_ (cclient clients) $ \client -> do
--     -- forM_ clients $ \client -> do
--         return ()
--     where
--     cclient = L.find $ (event_ButtonPressEvent e ==) . (^. xid)

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

data Axis = X | Y deriving (Show, Typeable)

edgeToAxis :: Edge -> Axis
edgeToAxis edge
    | edge == North || edge == South = Y
    | edge == East  || edge == West  = X


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
    result lst = if L.null lst then Nothing else Just $ minmax lst

    borders = result
            $ filter borderp
            $ map (flip clientBorder $ oppositeEdge edge)
            $ filter clientp
            $ clients

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

    north   = cy
    south c = cy c + fi (ch c)
    east  c = cx c + fi (cw c)
    west    = cx

    cx c = c ^. geometry . position . x
    cy c = c ^. geometry . position . y
    cw c = c ^. geometry . dimension . width
    ch c = c ^. geometry . dimension . height

    minmax
        | edge == North || edge == West = maximum
        | edge == South || edge == East = minimum


closestBorders :: [Client] -> Client -> [(Edge, Int)]
closestBorders cs c = catMaybes $ map cb [North, South, East, West]
    where cb e = fmap (e,) (closestBorder cs c e)


closestBordersInDirection :: [(Edge, Int)]
                          -> (Maybe Edge, Maybe Edge)
                          -> (Maybe (Edge, Int), Maybe (Edge, Int))
closestBordersInDirection es (e1, e2) = (e1 >>= try es, e2 >>= try es)
    where
    try []             _ = Nothing
    try ((e', b'):ebs) e | e == e' = Just (e, b')
                         | otherwise = try ebs e


bordersToList :: (Maybe (Edge, Int), Maybe (Edge, Int))
              -> [(Edge, Int)]
bordersToList (e1, e2) = maybe [] (:[]) e1 ++ maybe [] (:[]) e2


-- update :: Position -> (Maybe (Edge, Int), Maybe (Edge, Int)) -> Z PointerStack ()
-- update p (eb1, eb2) = modify update'
--     where
--     -- update' = undefined
--     update' Nothing = Just $ Lock p (eb1, eb2)
--     update' (Just (Lock p (eb1', eb2'))) = Just $ Lock p ((eb1 >>= flip update''' eb1')
--                                                          ,(eb1 >>= flip update''' eb1'))
--    -- update' (Just (Lock p (Nothing, Nothing))) = Just $ Lock p e1 e2
--     -- update' (Just (Lock p      e' Nothing)) = Just $ Lock p e' e2
--     -- update' (Just (Lock p Nothing       e')) = Just $ Lock p e1 e'
--     -- -- update' (Just (Lock p (Just (e', b) ey))
--     --     -- | Just $ Lock p e1 ey


-- update''' :: (Edge, Int) -> Maybe (Edge, Int) -> Maybe (Edge, Int)
-- update''' eb Nothing    = Just eb
-- update''' eb (Just eb') = update'' eb eb'

-- update'' :: (Edge, Int) -> (Edge, Int) -> Maybe (Edge, Int)
-- update'' (e, b) (e', b')
--     -- | e' == e                   = Just (e', b')
--     | e' == oppositeEdge e      = Nothing
--     | otherwise                 = Just (e', b')

-- matches :: ((Edge, Int) -> Bool)
--         -> (Maybe (Edge, Int), Maybe (Edge, Int))
--         -> [(Edge, Int)]
-- matches f (e1, e2) = match e1 ++ match e2
--     where
--     match Nothing = []
--     match (Just eb) = if f eb then [eb] else []


isResist' :: Int -> Int -> (Edge, Int) -> Bool
isResist' d b = pred
    where -- pred b' = abs (b' - b) < d
          pred (edge, b') | edge == North || edge == West = b <= b' && b' - b < d
                          | edge == South || edge == East = b >= b' && b - b' < d
               -- | edge == East  = b - b' < d
               -- | edge == West  = b' - b < d


isResist :: Int -> Int -> Maybe (Edge, Int) -> Maybe (Edge, Int)
isResist d b = (match =<<)
    where match eb = if pred eb then Just eb else Nothing
          pred (edge, b') | edge == North || edge == West = b <= b' && b' - b < d
                          | edge == South || edge == East = b >= b' && b - b' < d
               -- | edge == East  = b - b' < d
               -- | edge == West  = b' - b < d


resistLock :: Maybe (Edge, Int) -> Maybe (Edge, Int) -> Maybe (Edge, Int)
resistLock Nothing Nothing = Nothing
resistLock eb      Nothing = eb
resistLock Nothing eb      = eb
resistLock (Just (e, b)) (Just (e', _)) | e == e'   = Just (e, b)
                                        | otherwise = Nothing

-- lockX p [] = return ()
-- lockX p ((e, x):_) = modify updateX
--     where
--     updateX Nothing             = Just $ Lock p (Just (e, x)) Nothing
--     updateX (Just (Lock p _ y)) = Just $ Lock p (Just (e, x)) y

-- lockY p [] = return ()
-- lockY p ((e, y):_) = modify updateY
--     where
--     updateY Nothing             = Just $ Lock p Nothing (Just (e, y))
--     updateY (Just (Lock p x _)) = Just $ Lock p x       (Just (e, y))


lockX p x = modify updateX
    where
    updateX Nothing             = Just $ Lock p x Nothing
    updateX (Just (Lock p _ y)) = Just $ Lock p x y

lockY p y = modify updateY
    where
    updateY Nothing             = Just $ Lock p Nothing y
    updateY (Just (Lock p x _)) = Just $ Lock p x y

{-
closestBorder :: [Client] -> Client -> Edge -> Maybe Int
closestBorder clients client = \case
    North -> result maximum south $ filterClients ((north client >) . south) clients
    South -> result minimum north $ filterClients ((south client <) . north) clients
    East  -> result minimum west  $ filterClients ((east  client <) . west ) clients
    West  -> result maximum east  $ filterClients ((west  client >) . east ) clients

    where
    result _ _       []  = Nothing
    result f borderp lst = Just . f . map (fi . borderp) $ lst
    filterClients pred = filter (\c -> c /= client && pred c)

    -- filtered = \case
    --     North -> filter (flip pred North) clients
    --     South -> filter (flip pred South) clients
    --     East  -> filter (flip pred East ) clients
    --     West  -> filter (flip pred West ) clients

    -- pred c = \case
    --     North -> border client North > border c South
    --     South -> border client South < border c North
    --     East  -> border client  East < border c  West
    --     West  -> border client  West > border c  East

    -- north_bs = map (flip border North) clients
    -- south_bs = map (flip border South) clients
    -- east_bs  = map (flip border  East) clients
    -- west_bs  = map (flip border  West) clients

    north   = fi . (^. geometry . position . y)
    south c = fi $ north c + (c ^. geometry . dimension . height)
    east  c = fi $ west  c + (c ^. geometry . dimension . width)
    west    = fi . (^. geometry . position . x)
-}


direction :: Position -> Position -> (Maybe Edge, Maybe Edge)
direction from to = (x_direction delta_x, y_direction delta_y)
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


-- resistX' :: (Edge, Int) -> Maybe (Edge, Int) -> Maybe (Edge, Int)
-- resistX' ei Nothing = Just ei
-- resistX' ei ei' = fmap (resistX ei) ei'

-- updateY :: Edge -> Edge -> Int -> Just Int
-- updateY North North v = Just v
-- updateY North _     _ = Nothing
-- updateY _     _     _ = Nothing

resistX' :: Maybe (Edge, Int) -> Maybe (Edge, Int) -> Maybe (Edge, Int)
-- resistY' Nothing Nothing = Nothing
resistX' Nothing ei = ei
resistX' ei Nothing = ei
resistX' (Just ei) (Just ei') = Just $ resistY ei ei'
    -- | e == e' = Just (e, i)
    -- | otherwise = Nothing

resistY' :: Maybe (Edge, Int) -> Maybe (Edge, Int) -> Maybe (Edge, Int)
-- resistY' Nothing Nothing = Nothing
resistY' Nothing _  = Nothing
resistY' ei Nothing = ei
resistY' (Just (e, i)) (Just (e', i'))
    | e == e' = Just (e, i)
    | otherwise = Nothing

resist d p (North, v) = if p < v && v - p < d then Just (North,v) else Nothing
resist d p (South, v) = if p > v && p - v < d then Just (South,v) else Nothing
resist d p (East,  v) = if p > v && p - v < d then Just (East, v) else Nothing
resist d p (West,  v) = if p < v && v - p < d then Just (West, v) else Nothing

    -- fmap (resistY ei) ei'

    -- where pred = v <= v' && v' - v < p

-- resistX' ei Nothing = Just ei
-- resistX' ei (Just ei') = Just $ resistX ei ei'

resistY :: (Edge, Int) -> (Edge, Int) -> (Edge, Int)
resistY (North, b) (South, _) = (North, b)
resistY (South, b) (North, _) = (South, b)
resistY ei _ = ei
    -- | e == e' = (e, b)
    -- | otherwise = (e', b')

resistX :: (Edge, Int) -> (Edge, Int) -> (Edge, Int)
resistX (East, b) (East, _) = (East, b)
resistX (East, _) (West, b') = (West, b')
resistX ei _ = ei

    -- | e == e' = (e, b)
    -- | otherwise = (e', b')

-- resistX :: Maybe (Edge, Int) -> Maybe (Edge, Int) -> Maybe (Edge, Int)
-- resistX (East, b)
--     | e == e'
-- resistX West bx
-- resistX _ = Nothing

checkBoundary :: (Int -> Bool) -> Int -> Maybe Int
checkBoundary p i
    | p i       = Just i
    | otherwise = Nothing


moveResist :: PointerMotion -> MotionNotifyEvent -> Z PointerStack ()
-- moveResist (MR ppos last_x last_y) e = do
moveResist (Lock ppos lock_x lock_y) e = do
    clients <- maybe [] getClientsReply <$> sendMessage GetClients
    -- state <- get
    -- toLog $ "clients: " ++ show (map (^. geometry . position) clients)
    toLog $ "clients: " ++ show clients
    whenJustM_ (L.find (\c -> c ^. xid == window) clients) $ flip move clients

    -- whenJustM_ (cclient clients) $ \client -> do
    -- -- forM_ clients $ \client -> do
    --     return ()

    where
    -- cclient = L.find $ (event_MotionNotifyEvent e ==) . (^. xid)

    -- handle (Just (M (Position x' y'))) = do
    --     x' 

    root_x = root_x_MotionNotifyEvent e
    root_y = root_y_MotionNotifyEvent e
    -- event_x = event_x_MotionNotifyEvent e
    -- event_y = event_y_MotionNotifyEvent e
    window = event_MotionNotifyEvent e



    -- resist' :: Int -> Int -> Maybe Int -> Int
    -- resist' _ _ Nothing   = False
    -- resist' p v (Just v') = pred
    --     where pred = v <= v' && v' - v < p

    -- -- Pred, client border, other border
    -- resist2 :: Int -> Int -> Int -> Int
    -- resist2 p v v' = if pred then v' else v
    --     where pred = v <= v' && v' - v < p

    -- resist :: Int -> Int -> Maybe Int -> Int
    -- resist _ v Nothing   = v
    -- resist p v (Just v') = if pred then v' else v
    --     where pred = v <= v' && v' - v < p

    -- snapping?
    -- resist p v (Just v') = if abs (v - v') < p then v' else v

    -- lock :: Int -> Int -> Maybe (Edge, Int) -> Maybe (Edge, Int)
    -- lock _ _ Nothing      = Nothing
    -- lock p v (Just (e, v')) = if pred then Just (e, v') else Nothing
    --     where pred = v <= v' && v' - v < p

    -- update f (Just mr) = Just $ f mr
    -- update _ Nothing   = Nothing

    -- update (MR p (rx, ry)) =  MR p (x_border, ry)))

    -- update (x_dir, x_res) (y_dir, y_res) (MR p (Just lrx) (Just lry))
    --     | x_dir == fst lrx = MR p lrx lry
    --     | x_dir /= fst lrx = MR p Nothing lry
    --     | y_dir == fst lry = MR p lrx (y_dir, y_res)
    --     | y_dir /= fst lry = MR p lrx Nothing

    move cclient clients = do
        -- distance <- asks (resistDistance)
        let distance = 30
        border_width <- askL $ config . borderWidth

        let new_position = Position (fi root_x - ppos ^. x)
                                    (fi root_y - ppos ^. y)
            directions = direction (cclient ^. geometry . position) new_position

            -- border e = closestBorder clients cclient e >>= checkBoundary resist
            -- Maybe (Edge, Int)
            border e = (e,) . (+ 2 * fi border_width) <$> closestBorder clients cclient e

            -- cy = fromMaybe (new_position ^. y) (y_border)
            new_px = (new_position ^. x)
            new_py = (new_position ^. y)

            -- px' = last_x >>= resist distance new_px
            -- py' = last_y >>= resist distance new_py

            -- px'' = if isJust px' then px' else x_border
            -- py'' = if isJust py' then py' else y_border


        toLog $ "directions: " ++ show directions
        -- toLog $ "closestBorder (y): " ++ show (snd directions >>= border)
        -- toLog $ "x_border: " ++ show x_border
        -- toLog $ "last_y: " ++ show last_y
        -- toLog $ "y_border: " ++ show y_border
        -- toLog $ "y_border': " ++ show y_border'
        -- toLog $ "py': " ++ show py'
        -- toLog $ "py'': " ++ show py''
        toLog $ "new_px, new_py: " ++ show new_px ++ ", " ++ show new_py

        let closestX = (fst directions >>= border)
            closestY = (snd directions >>= border)
            -- isResistX = isResist distance new_px (fst directions >>= border)
            -- isResistY = isResist distance new_py (snd directions >>= border)

            isResistX' = isResist' distance new_px
            isResistY' = isResist' distance new_py

            -- cx = if isResistX then snd $ fromJust closestX else new_px
            -- cy = if isResistY then snd $ fromJust closestY else new_py

            resistLockX = resistLock (fmap (,new_px) $ fst directions) lock_x
            resistLockY = resistLock (fmap (,new_py) $ fst directions) lock_y

            matchX (e, _) = fromMaybe False $ fmap (== e) $ fst directions
            matchY (e, _) = fromMaybe False $ fmap (== e) $ snd directions

            applyBorderWidth (e, b)
                | e == North || e == West = (e, b + 2 * fi border_width)
                | otherwise               = (e, b - 2 * fi border_width)
            -- cbs = map (second (2 * fi border_width +)) (closestBorders clients cclient)
            cbs = map applyBorderWidth (closestBorders clients cclient)
            cbsid = closestBordersInDirection cbs directions
            cbsid' = first (isResist distance new_px)
                   $ second (isResist distance new_py) cbsid


            -- cbsX = filter (\ei -> matchX ei && isResistX' ei) $ closestBorders clients cclient
            -- cbsY = filter (\ei -> matchY ei && isResistY' ei) $ closestBorders clients cclient

            -- cx' = whenJust (fst cbsid) $ \(e, b) ->
            --     if (b == clientBorder cclient e)
            --         then if (b - new_px) < distance then b else new_px
            --         else new_px

            -- cy' = whenJust (snd cbsid) $ \(e, b) ->
            --     if (b == clientBorder cclient e)
            --         then if (b - new_py) < distance then b else new_py
            --         else new_py

            check b' (e, b) =
                if (b == clientBorder cclient e)
                    then if pred
                        then Just border
                        else Nothing
                    else Nothing

                where pred | e == North || e == West = b - b' < distance
                           | e == South = b' - (b - client_height) < distance
                           | e == East  = b' - (b - client_width ) < distance

                      border | e == North || e == West = b
                             | e == South              = b - client_height
                             | e == East               = b - client_width

                      -- is_valid | e == North || e == West = b == clientBorder cclient e
                      --          | e == South                b == clientBorder cclient e
                      --          | e == East                 b == clientBorder cclient e

                      -- true_y = b - client_height
                      -- true_x = b - client_height
                      client_width  = fi $ cclient ^. geometry . dimension . width
                      client_height = fi $ cclient ^. geometry . dimension . height

            cx' = fromMaybe new_px (fst cbsid >>= check new_px)
            cy' = fromMaybe new_py (snd cbsid >>= check new_py)

            update  Nothing   Nothing  = id
            update (Just x')  Nothing  = geometry . position . x .~ x'
            update  Nothing  (Just y') = geometry . position . y .~ y'
            update (Just x') (Just y') = (geometry . position . x .~ x')
                                       . (geometry . position . y .~ y')

            -- config_x = [(ConfigWindowX, fi new_px)]
            -- config_y = [(ConfigWindowY, fi new_py)]

            -- config_x = maybe [(ConfigWindowX, fi new_px)] ((:[]) . (ConfigWindowX,) . fi) cx'
            -- config_y = maybe [] ((:[]) . (ConfigWindowY,) . fi) cy'
            config_x = [(ConfigWindowX, fi cx')]
            config_y = [(ConfigWindowY, fi cy')]

            -- config_x = maybe [] ((:[]) . (ConfigWindowX,) . fi) cx'
            -- config_y = maybe [] ((:[]) . (ConfigWindowY,) . fi) cy'

        -- when (isJust lock_x || isResistX) $ lockX ppos resistLockX -- (fmap (,new_px) $ fst directions)
        -- when (isJust lock_y || isResistY) $ lockY ppos resistLockY -- (fmap (,new_py) $ snd directions)

        toLog $ "FFF: " ++ show ( map (clientBorder cclient) [North, South, East, West])
        -- toLog $ "UUU: " ++ show 

        toLog $ "cclient: " ++ show cclient
        -- toLog $ "lock_x: " ++ show lock_x
        -- toLog $ "lock_y: " ++ show lock_y
        -- toLog $ "cbsY: " ++ show cbsY
        toLog $ "cbs: " ++ show cbs
        toLog $ "cbsid: " ++ show cbsid
        -- toLog $ "cbsid': " ++ show cbsid'
        toLog $ "cx': " ++ show cx' ++ "; cy': " ++ show cy'
        toLog $ "config_x ++ config_y: " ++ show (config_x ++ config_y)

        -- let cx = fromMaybe new_px . fmap snd
        --        $ if (isNothing $ fst cbsid') then lock_x else fst cbsid'
        --     cy = fromMaybe new_py . fmap snd
        --        $ if (isNothing $ snd cbsid') then lock_y else snd cbsid'

        -- toLog $ "cx: " ++ show cx
        -- toLog $ "cy: " ++ show cy

        -- when (isJust $ fst cbsid') $ lockX ppos (fst cbsid')
        -- when (isJust $ snd cbsid') $ lockY ppos (snd cbsid')

        -- -- whenJustM_ (fst cbsid') $ \(e, b) ->
        -- whenJustM_ (fst directions) $ \e -> do
        --     whenJustM_ (lock_x) $ \(e', b') ->
        --         when (e == oppositeEdge e') $ lockX ppos Nothing

        -- whenJustM_ (snd directions) $ \e -> do
        --     toLog "whenJustM_ 1"
        --     whenJustM_ (lock_y) $ \(e', _) -> do
        --         toLog "whenJustM_ 2"
        --         when (e == oppositeEdge e') $ lockY ppos Nothing

        -- let np = Position cx cy
        void (sendMessage
              -- (UpdateClient window $ geometry . position .~ np)
              -- (UpdateClient window $ update cx' cy')
              (UpdateClient window $ update (Just cx') (Just cy'))
                :: Z PointerStack (Maybe CoreMessageReply))


        {-
        if (fst cbsid') == Nothing
            then tryLock
            else updateLock

        updateLock = put!

        let isLockedX = lock_x && fst cbsid'
        if isLockedX then (snd lock_x) else new_px

        if isLockedX
            then checkLock lockX (fst cbsid')
            else return ()

        checkLock = if e1 /= e2 -> Nothing
        -}

        -- toLog $ "isResistX: " ++ show (isResist distance new_px (fst cbsid))
        -- toLog $ "isResistY: " ++ show (isResist distance new_py (snd cbsid))

        -- toLog $ "closestBorders: " ++ show (closestBorders clients cclient)
        -- toLog $ "resistLockX: " ++ show resistLockX -- (resistLock closestX lock_x)
        -- toLog $ "resistLockY: " ++ show resistLockY -- (resistLock closestY lock_y)
        -- toLog $ "lock_x: " ++ show lock_x ++ "; lock_y: " ++ show lock_y
        -- toLog $ "closestX: " ++ show closestX ++ "; closestY: " ++ show closestY
        -- toLog $ "isResistX: " ++ show isResistX ++ "; isResistY: " ++ show isResistY


        -- put . Just $ MR ppos x_border py''

        -- let cx = if L.null cbsX then new_px else snd $ head cbsX
        --     cy = if L.null cbsY then new_py else snd $ head cbsY

        W.configure window $ config_x ++ config_y
                            -- [(ConfigWindowX, fi cx),
                            -- (ConfigWindowY, fi cy)]

    {-
    client_x      = fi . (^. geometry . position . x)
    client_y      = fi . (^. geometry . position . y)
    client_width  = fi . (^. geometry . dimension . width)
    client_height = fi . (^. geometry . dimension . height)

    north_border   = client_y
    south_border c = client_y c + client_height c
    east_border c  = client_x c + client_width c
    west_border    = client_x

    delta_x src_x = root_x - src_x
    delta_y src_y = root_y - src_y

    x_direction delta
        | delta > 0 = Just East
        | delta < 0 = Just West
        | otherwise = Nothing

    y_direction delta
        | delta < 0 = Just North
        | delta > 0 = Just South
        | otherwise = Nothing

    distance dir c1 c2
        | dir == Just North = result $ north_border c1 - south_border c2
        | dir == Just South = result $ south_border c1 - north_border c2
        | dir == Just East  = result $  east_border c1 - west_border c2
        | dir == Just West  = result $  west_border c1 - east_border c2
        | otherwise = Nothing
        where result x | x > 0     = Just x
                       | otherwise = Nothing

    distances r _       []               = r
    distances r cclient (client:clients) =
        let x_dir = x_direction . delta_x . fi $ cclient ^. geometry . position . x
            y_dir = y_direction . delta_y . fi $ cclient ^. geometry . position . y
            x_distance = distance x_dir cclient client
            y_distance = distance y_dir cclient client
            in (x_distance, y_distance) : r

    update x' y' c = c & geometry . position . x .~ x'
                       & geometry . position . y .~ y'
    -}
    {-
    move Nothing        _       _                = return ()
    move (Just cclient) clients (Position x' y') = do

        let dists = distances [] cclient clients
        let min_x = minimum . map fst $ distances [] cclient clients
            min_y = minimum . map snd $ distances [] cclient clients
            -- config_x = fi $ fromMaybe (root_x - fi x') min_x
            config_y' = fi $ fromMaybe (root_y - fi y') min_y
            config_x = fi (root_x - fi x')
            config_y = fi (root_y - fi y')

        let x_dir = x_direction . delta_x . fi $ cclient ^. geometry . position . x
            y_dir = y_direction . delta_y . fi $ cclient ^. geometry . position . y

        toLog $ "x_dir: " ++ show x_dir
        toLog $ "y_dir: " ++ show y_dir
        toLog $ "config_y: " ++ show config_y
        toLog $ "distances: " ++ show dists
        toLog $ "min_y: " ++ show min_y
        toLog $ "config_y': " ++ show config_y'
        void (sendMessage (UpdateClient window $ update config_x config_y) :: Z PointerStack (Maybe CoreMessageReply))
        W.configure window [(ConfigWindowX, fi config_x), (ConfigWindowY, fi config_y)]
    -}



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
