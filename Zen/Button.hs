-- vim:sw=4:sts=4:ts=4

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TupleSections      #-}

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
import Types hiding (Raise, Lower)
import qualified Queue as Q
import qualified Model as Model
import Keyboard (getCleanMask, extraModifier)
import SnapResist (moveSnapResist)


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
    { buttonActions :: ButtonMap
    }
    deriving (Eq, Show, Typeable)


type GlyphMap = Map Glyph CURSOR

data PointerSetup = PointerSetup
    { buttonConfig :: ButtonConfig
    , buttonMask :: [EventMask]
    , glyphMap :: GlyphMap
    }
    deriving (Show, Typeable)


data PointerMotion = M Position Position
                   | R (Maybe Direction, Maybe Direction) Position Geometry
    deriving (Show, Typeable)

type PointerStackT = ReaderT PointerSetup (StateT (Maybe PointerMotion) (Z IO))

putPM :: (MonadState (Maybe PointerMotion) m) => Maybe PointerMotion -> m ()
putPM = put


getPM :: (MonadState (Maybe PointerMotion) m, MonadTrans t, Monad (t m)) => t m (Maybe PointerMotion)
getPM = lift get

asksPS :: (MonadReader PointerSetup m) => (PointerSetup -> a) -> m a
asksPS = asks


pointerComponent :: ButtonConfig -> ControllerComponent
pointerComponent buttons = Component
    { componentId = "Pointer"
    , componentData = (PointerSetup buttons [] M.empty, Nothing)
    , execComponent = execPointerComponent
    , onStartup = startupPointerComponent
    , onShutdown = shutdownPointerComponent
    , someHandler = const $ map SomeHandler
                          [ EventHandler handleButtonPress
                          , EventHandler handleMotionNotify
                          , EventHandler handleCreateNotify
                          ]
    }


execPointerComponent :: PointerStackT a
                     -> (PointerSetup, Maybe PointerMotion)
                     -> Z IO (PointerSetup, Maybe PointerMotion)
execPointerComponent f (ps, pm) = (ps,) <$> execStateT (runReaderT f ps) pm


startupPointerComponent :: (PointerSetup, Maybe PointerMotion)
                        -> Z IO (PointerSetup, Maybe PointerMotion)
startupPointerComponent (PointerSetup buttons bm _, p) = connection $-> \c -> do
    toLog "startupPointerComponent"
    glyphs <- io (withFont c "cursor" $ flip (loadGlyphCursors c) cursorGlyphs)

    Model.withQueueM $ mapM_ (flip grabButtons (buttonActions buttons))
                     . map (^. xid) . Q.toList

    return (PointerSetup buttons buttonEventMask glyphs, p)


shutdownPointerComponent :: (PointerSetup, Maybe PointerMotion) -> Z IO ()
shutdownPointerComponent (PointerSetup _ _ glyphs, _) = do
    toLog "shutdownPointerComponent"
    connection $-> \c -> mapM_ (io . freeCursor c) (M.elems glyphs)


handleButtonPress :: ButtonPressEvent -> PointerStackT ()
handleButtonPress e = do
    toLog "ButtonPressEvent"

    mask <- (\\) <$> (lift . lift $ getCleanMask bstate) <*> askModMask
    asksPS (M.lookup (mask, button) . buttonActions . buttonConfig) >>= flip whenJustM_ handle

    where
    bstate = state_ButtonPressEvent e
    button = fromValue $ detail_ButtonPressEvent e

    askModMask = lift . lift $ askL (config . modMask)

    handle :: PointerAction -> PointerStackT ()
    handle = \case
        (Move) -> doMove e
        (Resize)    -> doResize e
        (Raise)     -> doRaise e
        (Lower)     -> doLower e


doRaise :: ButtonPressEvent -> PointerStackT ()
doRaise = Model.raise . event_ButtonPressEvent


doLower :: ButtonPressEvent -> PointerStackT ()
doLower = Model.lower . event_ButtonPressEvent


doMove :: ButtonPressEvent -> PointerStackT ()
doMove e = do
    doRaise e
    putPM . Just $ M (Position event_x event_y) (Position root_x root_y)
    -- put $ Just . M $ Position root_x root_y
    flip whenJustM_ changeCursor =<< asksPS (M.lookup xC_fleur . glyphMap)
    where
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e


doResize :: ButtonPressEvent -> PointerStackT ()
doResize e = do
    doRaise e
    Model.lookup window >>= flip whenJustM_ resize

    where
    window = event_ButtonPressEvent e
    root_x = fi $ root_x_ButtonPressEvent e
    root_y = fi $ root_y_ButtonPressEvent e
    event_x = fi $ event_x_ButtonPressEvent e
    event_y = fi $ event_y_ButtonPressEvent e
    edges = getEdges . Geometry (Position event_x event_y) . (^. geometry . dimension)

    resize c = do
        putPM $ Just $ R (edges c) (Position root_x root_y) (c ^. geometry)
        flip whenJustM_ changeCursor
            =<< asksPS (M.lookup (getCursor $ edges c) . glyphMap)


moveMotionNotify :: MotionNotifyEvent -> PointerStackT ()
moveMotionNotify e = getPM >>= \case
    Just m -> do
        let bw = 3
        mclient <- Model.lookup window
        clients <- map (addbw bw) <$> Model.toList
        whenJustM_ mclient $ doMoveMotionNotify e m clients . addbw bw
    _ -> return ()
    where window = event_MotionNotifyEvent e
          addbw bw = -- (geometry . position . x -~ bw)
                   -- . (geometry . position . y -~ bw)
                     (geometry . dimension . width +~ 2 * bw)
                   . (geometry . dimension . height +~ 2 * bw)


doMoveMotionNotify :: MotionNotifyEvent
                   -> PointerMotion
                   -> [Client]
                   -> Client
                   -> PointerStackT ()
doMoveMotionNotify e (M epos rpos) clients client = do
    lift . lift $ moveSnapResist e rpos epos client clients

    -- put . Just $ M epos rpos
    putPM . Just $ M epos $ Position (fi root_x) (fi root_y)
    -- put . Just $ M (Position (fi event_x) (fi event_y)) rpos
    -- put . Just $ M (Position (fi event_x) (fi event_y)) (Position (fi root_x) (fi root_y))

    where root_x = root_x_MotionNotifyEvent e
          root_y = root_y_MotionNotifyEvent e

{-
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
-}






handleMotionNotify :: MotionNotifyEvent -> PointerStackT ()
handleMotionNotify e = getPM >>= handle
    where
    handle :: Maybe PointerMotion -> PointerStackT ()
    handle Nothing              = return ()
    handle (Just (M _ _))         = moveMotionNotify e
    -- handle (Just (M p))         = W.configure window
    --     $ [(ConfigWindowX, root_x - src_x p), (ConfigWindowY, root_y - src_y p)]
    handle (Just r@(R edges p g)) = do
        updateClient (fst edges) p g
        updateClient (snd edges) p g

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

    updateClient edge p g = case edge of
        Just North -> do Model.setY      window $ win_y g + delta_y p
                         Model.setHeight window $ win_h g - fi (delta_y p)
        Just South -> do Model.setHeight window $ win_h g + fi (delta_y p)
        Just East  -> do Model.setWidth  window $ win_w g + delta_x p
        Just West  -> do Model.setX      window $ win_x g + fi (delta_x p)
                         Model.setWidth  window $ win_w g - delta_x p
        _          -> return ()


handleCreateNotify :: CreateNotifyEvent -> PointerStackT ()
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    mask <- asksPS buttonMask
    buttons <- asksPS (buttonActions . buttonConfig)
    whenM isClient $ lift . lift $ grabButtons window buttons
    where
    window = window_CreateNotifyEvent e
    -- isClient = maybe False isClientReply <$> sendMessage (IsClient window)
    isClient = Model.member window


grabButtons :: MonadIO m => WindowId -> ButtonMap -> Z m ()
grabButtons w = mapM_ (uncurry . flip $ Model.grabButton w) . M.keys


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


changeCursor :: CURSOR -> PointerStackT ()
changeCursor cursor = askConnection >>= \c -> asksPS buttonMask >>= changeGrab c
    where
    askConnection = lift . lift $ askL connection
    changeGrab c = liftIO . changeActivePointerGrab c
                          . MkChangeActivePointerGrab cursor
                                                      (toValue TimeCurrentTime)
