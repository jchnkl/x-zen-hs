{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             LambdaCase,
             TupleSections #-}

module Button where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import Data.Typeable
import Control.Exception (bracket)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow (second)
import Control.Applicative ((<*>), (<$>))
import Graphics.XHB
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor

import Log
import Util
import Lens
import Types
import Window
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


data PointerAction = Move | Resize | Raise | Lower
    deriving (Eq, Show, Typeable)

type ButtonMap = Map ([ModMask], ButtonIndex) PointerAction

data ButtonConfig = ButtonConfig
    { _buttonActions :: ButtonMap
    }
    deriving (Eq, Show, Typeable)

buttonActions :: Z PointerStack (Maybe ButtonMap)
buttonActions = (_buttonActions <$>)
            <$> asksL (config . componentConfigs) getButtonConfig


type GlyphMap = Map Glyph CURSOR

data PointerSetup = PointerSetup
    { buttonMask :: [EventMask]
    , glyphMap :: GlyphMap
    }
    deriving Typeable


data PointerMotion = M Position
                   | R (Maybe Edge, Maybe Edge) Position Geometry
    deriving (Show, Typeable)

type PointerStack = ReaderT PointerSetup (StateT (Maybe PointerMotion) IO)


asksPS :: (PointerSetup -> a) -> Z PointerStack a
asksPS = lift . lift . asks

pointerComponent :: Component
pointerComponent = Component
    { component = (PointerSetup [] M.empty, Nothing)
    , runComponent = runPointerComponent
    , startup = startupPointerComponent
    , cleanup = cleanupPointerComponent
    , handleEvent = eventDispatcher [ EventHandler handleButtonPress
                                    , EventHandler handleMotionNotify
                                    , EventHandler handleCreateNotify
                                    ]
    , handleMessage = (\_ -> return ())
    }


runPointerComponent :: PointerStack a
                    -> (PointerSetup, Maybe PointerMotion)
                    -> IO (a, (PointerSetup, Maybe PointerMotion))
runPointerComponent f (ps, pm) = second (ps,) <$> runStateT (runReaderT f ps) pm


startupPointerComponent :: (PointerSetup, Maybe PointerMotion)
                           -> Z IO (PointerSetup, Maybe PointerMotion)
startupPointerComponent (PointerSetup m _, p) = connection $-> \c -> do
    glyphs <- io (withFont c "cursor" $ flip (loadGlyphCursors c) cursorGlyphs)
    return (PointerSetup m glyphs, p)


cleanupPointerComponent :: (PointerSetup, Maybe PointerMotion) -> Z IO ()
cleanupPointerComponent (PointerSetup _ glyphs, _) =
    connection $-> \c -> mapM_ (io . freeCursor c) (M.elems glyphs)


getButtonConfig :: [ComponentConfig] -> Maybe ButtonConfig
getButtonConfig = getConfig


handleButtonPress :: ButtonPressEvent -> Z PointerStack ()
handleButtonPress e = do
    toLog "ButtonPressEvent"

    mask <- (\\) <$> (getCleanMask bstate) <*> askL (config . modMask)
    flip whenJustM_ handle =<<
        (join . (M.lookup (mask, button) <$>) <$> buttonActions)

    where
    bstate = state_ButtonPressEvent e
    button = fromValue $ detail_ButtonPressEvent e

    handle :: PointerAction -> Z PointerStack ()
    handle = \case
        Move   -> doMove e
        Resize -> doResize e
        Raise  -> doRaise e
        Lower  -> doLower e


doRaise :: ButtonPressEvent -> Z PointerStack ()
doRaise = raise . event_ButtonPressEvent

doLower :: ButtonPressEvent -> Z PointerStack ()
doLower = lower . event_ButtonPressEvent

doMove :: ButtonPressEvent -> Z PointerStack ()
doMove e = do
    doRaise e
    put $ Just . M $ Position event_x event_y
    flip whenJustM_ changeCursor =<< asksPS (M.lookup xC_fleur . glyphMap)
    where
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


handleMotionNotify :: MotionNotifyEvent -> Z PointerStack ()
handleMotionNotify e = get >>= handle
    where
    handle :: Maybe PointerMotion -> Z PointerStack ()
    handle Nothing              = return ()
    handle (Just (M p))         = configure window
        $ [(ConfigWindowX, root_x - src_x p), (ConfigWindowY, root_y - src_y p)]
    handle (Just (R edges p g)) = configure window
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
    toLog "CreateNotifyEvent"
    grabButtons $ window_CreateNotifyEvent e


grabButtons :: WindowId -> Z PointerStack ()
grabButtons window = connection $-> \c -> whenM (isClient window) $ do
    toLog $ "grabButtons for " ++ show window

    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    eventmask <- asksPS buttonMask
    buttons <- (fromMaybe [] . fmap M.keys) <$> buttonActions

    forM_ buttons $ \(m, b) -> do
        let keys = zip (combinations (m ++ modmask ++ extraModifier kbdmap modmap)) (repeat b)
        mapM_ (grab c eventmask) keys

    where
    grab c eventmask (mask, button) = do
        toLog $ "grabbing: " ++ show (mask, button)
        io $ grabButton c $ MkGrabButton True window eventmask
                            GrabModeAsync GrabModeAsync
                            (convertXid xidNone) (convertXid xidNone)
                            button mask


isClient :: (Functor m, MonadIO m) => WindowId -> Z m Bool
isClient window = check <$> attributes
    where
    attributes :: MonadIO m => Z m (Either SomeError GetWindowAttributesReply)
    attributes = io . getReply =<<
        io . flip getWindowAttributes window <-$ connection

    check :: Either SomeError GetWindowAttributesReply -> Bool
    check (Right reply) = not $ isUnviewable reply
    check _             = False

    isUnviewable :: GetWindowAttributesReply -> Bool
    isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r


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
