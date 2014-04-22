{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Client where

import Data.Word
-- import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Graphics.XHB
import Graphics.X11.Types hiding (Connection)

import Util
import Types

-- setClientPosition :: Client -> Position -> Client
-- setClientPosition client = undefined -- client ^. geometry ^. position ^=

-- setClientDimension :: Client -> Dimension -> Client
-- setClientDimension client d = undefined -- client { geometry = (geometry client) { dimension = d } }

withWindows :: ([Client] -> Z a) -> Z a
withWindows f = getsL (clients <.> queue) >>= f

-- withFocused :: (Client -> Z a) -> Z a
-- withFocused = undefined


focus :: Client -> Z ()
focus client = do
    toLog $ "focus: " ++ show (client ^. xid)
    let mk_setinputfocus = MkSetInputFocus InputFocusNone
                                           (client ^. xid)
                                           (toValue TimeCurrentTime)
    withConnection $ io . flip setInputFocus mk_setinputfocus
    getsL (focusedBorderColor <.> config) >>= setBorderColor client


unfocus :: Client -> Z ()
unfocus client = do
    toLog $ "unfocus: " ++ show (client ^. xid)
    getsL (normalBorderColor <.> config) >>= setBorderColor client


withClient :: WindowId -> (Client -> Z a) -> Z (Maybe a)
withClient w f = (L.find ((w ==) . getL xid))
    <$> (getsL (clients <.> queue))
        >>= flip whenJust f

modifyClient :: WindowId -> (Client -> Client) -> Z ()
modifyClient window f = clients <.> queue %:= update window f
    where
    update :: WindowId -> (Client -> Client) -> [Client] -> [Client]
    update w fun (c:cs)
        | w == c ^. xid = fun c : cs
        | otherwise = c : update w fun cs
    update _ _ _ = []

modifyQueue :: (Queue -> Queue) -> Z ()
modifyQueue f = modifyL queue f


setBorderColor :: Client -> Word32 -> Z ()
setBorderColor client bc = do
    c <- asksL connection
    io $ changeWindowAttributes c (client ^. xid)
                                  $ toValueParam [(CWBorderPixel, bc)]


setBorderWidth :: Client -> Z ()
setBorderWidth client = do
    bw <- getsL (borderWidth <.> config)
    let values = toValueParam [(ConfigWindowBorderWidth, fi $ bw)]
    withConnection $ \c -> io $ configureWindow c (client ^. xid) values


insertClient :: Client -> Z ()
insertClient client = do
    setBorderWidth client
    getsL (normalBorderColor <.> config) >>= setBorderColor client
    withConnection $ \c -> io $ changeWindowAttributes c (client ^. xid) attributes
    clients <.> queue %:= (client :)
    where
    attributes = toValueParam
        [(CWEventMask, toMask [EventMaskEnterWindow, EventMaskLeaveWindow])]


insertWindow :: WindowId -> Z ()
insertWindow window = do
    c <- asksL connection
    reply <- io (getWindowAttributes c window >>= getReply)
    case map_state_GetWindowAttributesReply <$> reply of
        Left _ -> return ()
        Right MapStateUnviewable -> return ()
        _ -> withConnection (io . flip getGeometry (convertXid window))
                >>= io . getReply
                    >>= updateQueue
    where
    updateQueue (Left _) = return ()
    updateQueue (Right geom) = do
        let x' = fi $ x_GetGeometryReply geom
            y' = fi $ y_GetGeometryReply geom
            w' = fi $ width_GetGeometryReply geom
            h' = fi $ height_GetGeometryReply geom
            c' = Client window (Geometry (Position x' y') (Dimension w' h'))
        insertClient c'


insertWindows :: [WindowId] -> Z ()
insertWindows windows = do
    c <- asksL connection
    filterValid <$> attributes c
        >>= fmap (>>= makeClients) . geometries c
            >>= insert

    where
    attributes :: Connection -> Z (Either SomeError [GetWindowAttributesReply])
    attributes c = io $ mapM (getWindowAttributes c) windows >>= getReplies

    geometries :: Connection -> [WindowId]
               -> Z (Either SomeError [(WindowId, GetGeometryReply)])
    geometries c ws = (Right . zip ws =<<)
        <$> io (mapM (getGeometry c . convertXid) ws >>= getReplies)

    filterValid :: Either SomeError [GetWindowAttributesReply] -> [WindowId]
    filterValid (Right attrs) = map fst $ filter isValid $ zip windows attrs
    filterValid _ = []

    isValid :: (WindowId, GetWindowAttributesReply) -> Bool
    isValid = not . override_redirect_GetWindowAttributesReply . snd

    makeClients :: [(WindowId, GetGeometryReply)] -> Either SomeError [Client]
    makeClients ((window, geom) : rest) = do
        let x' = fi $ x_GetGeometryReply geom
            y' = fi $ y_GetGeometryReply geom
            w' = fi $ width_GetGeometryReply geom
            h' = fi $ height_GetGeometryReply geom
            c' = Client window (Geometry (Position x' y') (Dimension w' h'))
        makeClients rest >>= Right . (c' :)
    makeClients _ = Right []

    insert :: Either SomeError [Client] -> Z ()
    insert (Right cs) = mapM_ insertClient cs
    insert _ = return ()


removeClient :: Client -> Z ()
removeClient client = clients <.> queue %:= (client `L.delete`)

deleteWindow :: WindowId -> [Client] -> [Client]
deleteWindow w (c:cs)
    | w == c ^. xid = cs
    | otherwise = c : deleteWindow w cs
deleteWindow _ _ = []

removeWindow :: WindowId -> Z ()
removeWindow window = clients <.> queue %:= (window `deleteWindow`)

raise :: WindowId -> Z ()
raise window = withConnection $ \c -> io $ configureWindow c window values
    where
    values = toValueParam [(ConfigWindowStackMode, toValue StackModeAbove)]

lower :: WindowId -> Z ()
lower window = withConnection $ \c -> io $ configureWindow c window values
    where
    values = toValueParam [(ConfigWindowStackMode, toValue StackModeBelow)]

grabButtons :: WindowId -> Z ()
grabButtons window = do
    c <- asksL connection
    mask <- getsL (modMask <.> config)
    pbs <- M.keys <$> getsL (buttonPressHandler <.> config)
    rbs <- M.keys <$> getsL (buttonReleaseHandler <.> config)
    forM_ (pbs `L.union` rbs) $ \button -> do
        io $ grabButton c $ MkGrabButton True window events
                                             GrabModeAsync GrabModeAsync
                                             (convertXid xidNone) (convertXid xidNone)
                                             button [mask]
    where
    events = [EventMaskButtonMotion, EventMaskButtonPress, EventMaskButtonRelease]

grabKeys :: Z ()
grabKeys = do
    c <- asksL connection

    let setup = connectionSetup c
        min_keycode = min_keycode_Setup setup
        max_keycode = max_keycode_Setup setup

    -- mask <- getsL (modMask <.> config)
    kbdmap <- io (keyboardMapping c =<<
        getKeyboardMapping c min_keycode (max_keycode - min_keycode + 1))

    -- modmap <- getModmap <$> io (getModifierMapping c >>= getReply)

    -- let numlock = join $ flip L.elemIndex modmap
    --  <$> (keysymToKeycode (fi xK_Num_Lock) kbdmap >>= \kc -> L.find (kc `elem`) modmap)
    --     capslock = join $ flip L.elemIndex modmap
    --  <$> (keysymToKeycode (fi xK_Caps_Lock) kbdmap >>= \kc -> L.find (kc `elem`) modmap)
    --     mask = map fromBit $ catMaybes [numlock, capslock]

    -- pbs <- M.keys <$> getsL (buttonPressHandler <.> config)
    -- rbs <- M.keys <$> getsL (buttonReleaseHandler <.> config)
    -- forM_ (pbs `L.union` rbs) $ \button -> do

    forM_ (map fi [xK_Alt_L]) $ \keysym -> do
        whenJust (keysymToKeycode keysym kbdmap) $ \keycode ->
            io $ grabKey c $ MkGrabKey True (getRoot c) [ModMaskAny] keycode
                                           GrabModeAsync GrabModeAsync

    where
    getModmap (Left _) = []
    getModmap (Right reply) =
        subLists (fi $ keycodes_per_modifier_GetModifierMappingReply reply)
                 (keycodes_GetModifierMappingReply reply)
