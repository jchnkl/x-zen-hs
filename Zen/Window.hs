{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Window where

import Data.Word
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad
-- import Control.Monad.State
import Control.Applicative
import Graphics.XHB
-- import Graphics.X11.Types hiding (Connection)

-- import Log
import Util
import Lens
-- import Core
-- import Config
-- import Setup
import Types hiding (focus)
import Queue


manage :: WindowId -> Z ()
manage window = whenM (isClient <$> attributes) $ do
    configure'
    queue %:= (insert $ Client window $ Position 0 0)

    where
    attributes :: Z (Either SomeError GetWindowAttributesReply)
    attributes = io . getReply =<<
        io . flip getWindowAttributes window <-$ connection

    isClient :: Either SomeError GetWindowAttributesReply -> Bool
    isClient (Right reply) = not $ isUnviewable reply
    isClient _             = False

    isUnviewable :: GetWindowAttributesReply -> Bool
    isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r

    configure' :: Z ()
    configure' = do
        let mask = CWEventMask
            values = toMask [EventMaskEnterWindow, EventMaskLeaveWindow]
            valueparam = toValueParam [(mask, values)]
        connection $-> \c -> io $ changeWindowAttributes c window valueparam
        borderWidth <.> config $-> setBorderWidth window
        grabButtons window


unmanage :: WindowId -> Z ()
unmanage w = queue %:= remove ((w ==) . getL xid)


setBorderColor :: WindowId -> Word -> Z ()
setBorderColor window bc = do
    let vp = toValueParam [(CWBorderPixel, fi bc)]
    connection $-> \c -> io $ changeWindowAttributes c window vp


setBorderWidth :: WindowId -> Word -> Z ()
setBorderWidth window bw = do
    let vp = toValueParam [(ConfigWindowBorderWidth, fi bw)]
    connection $-> \c -> io $ configureWindow c window vp


focus :: WindowId -> Z ()
focus window = do
    -- toLog $ "focus: " ++ show (client ^. xid)
    let mk_setinputfocus = MkSetInputFocus InputFocusNone
                                           window -- (client ^. xid)
                                           (toValue TimeCurrentTime)
    connection $-> io . flip setInputFocus mk_setinputfocus
    focusedBorderColor <.> config $-> setBorderColor window -- client


unfocus :: WindowId -> Z ()
unfocus window = do
    -- toLog $ "unfocus: " ++ show (client ^. xid)
    normalBorderColor <.> config $-> setBorderColor window -- client


configure :: WindowId -> [(ConfigWindow, Word32)] -> Z ()
configure w vs = connection $-> \c -> io $ configureWindow c w $ toValueParam vs
-- configure w vs = configure' w vs vs
--     where
--     configure' window vps' ((m, v):vps) = do
--         queue %:= with ((== window) . getL xid) (setValue m v)
--         configure' window vps' vps
--     configure' window vps' _ = do
--         connection $-> \c -> io $ configureWindow c window $ toValueParam vps'

    -- setValue ConfigWindowX v      = x <.> position <.> geometry ^= fi v
    -- setValue ConfigWindowY v      = y <.> position <.> geometry ^= fi v
    -- setValue ConfigWindowWidth v  = width <.> dimension <.> geometry ^= fi v
    -- setValue ConfigWindowHeight v = height <.> dimension <.> geometry ^= fi v
    -- setValue _ _                  = id


grabButtons :: WindowId -> Z ()
grabButtons window = connection $-> \c -> do
    modmask <- asksL (modMask <.> config)
    mapM_ (grab c modmask) =<< M.keys <$> asksL (buttonHandler <.> config)
    where
    events = [EventMaskButtonMotion, EventMaskButtonPress, EventMaskButtonRelease]
    grab c modmask (masks, button) =
        io $ grabButton c $ MkGrabButton True window events
                            GrabModeAsync GrabModeAsync
                            (convertXid xidNone) (convertXid xidNone)
                            button (modmask ++ masks)


    -- setValue ConfigWindowY v
    -- setValue ConfigWindowWidth v
    -- setValue ConfigWindowHeight v

-- configure window p d = queue %:= Q.map configure
--     where
--     configure :: Client -> Client
--     configure client
--         | client ^. xid == window = setClientGeometry p d client
--         | otherwise =  client


-- configureGeometry :: WindowId -> Maybe Position -> Maybe Dimension -> Z ()
-- configureGeometry window p d = queue %:= Q.map configure'
--     where
--     configure' :: Client -> Client
--     configure' client
--         | client ^. xid == window = setClientGeometry p d client
--         | otherwise =  client


-- setClientGeometry :: Maybe Position -> Maybe Dimension -> Client -> Client
-- setClientGeometry pos dim client = (geometry ^= (Geometry pos' dim')) client
--     where
--     pos' = fromMaybe (client ^. position <.> geometry) pos
--     dim' = fromMaybe (client ^. dimension <.> geometry) dim



-- modifyBy :: (Client -> Bool) -> (Client -> Client) -> Client -> Client
-- modifyBy p f client = if p client then f client else client



-- modify :: (Client -> Bool) -> (Client -> Client) -> [Client] -> [Client]
-- modify pred f (c:cs)
--     | pred c = f c : cs
--     | otherwise = c : modifyClient pred f cs
-- modify _ _ _ = []



{-
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
-}
