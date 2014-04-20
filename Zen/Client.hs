module Client where

import Data.Word
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad.State
import Control.Applicative
import Graphics.XHB

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
    let mk_setinputfocus = MkSetInputFocus InputFocusNone
                                           (client ^. xid)
                                           (toValue TimeCurrentTime)
    withConnection $ liftIO . flip setInputFocus mk_setinputfocus
    getsL (focusedBorderColor <.> config) >>= setBorderColor client


withClient :: ClientWindow -> (Client -> Z a) -> Z (Maybe a)
withClient w f = (L.find ((w ==) . getL xid))
    <$> (getsL (clients <.> queue))
        >>= flip whenJust f

modifyClient :: ClientWindow -> (Client -> Client) -> Z ()
modifyClient window f = clients <.> queue %:= update window f
    where
    update :: ClientWindow -> (Client -> Client) -> [Client] -> [Client]
    update w fun (c:cs)
        | w == c ^. xid = fun c : cs
        | otherwise = c : update w fun cs
    update _ _ _ = []

modifyQueue :: (Queue -> Queue) -> Z ()
modifyQueue f = modifyL queue f


setBorderColor :: Client -> Word32 -> Z ()
setBorderColor client bc = do
    c <- asksL connection
    liftIO $ changeWindowAttributes c (client ^. xid)
                                  $ toValueParam [(CWBorderPixel, bc)]


setBorderWidth :: Client -> Z ()
setBorderWidth client = do
    bw <- getsL (borderWidth <.> config)
    let cw = MkConfigureWindow (client ^. xid) mask valueparam
        mask = toMask [ConfigWindowBorderWidth]
        valueparam = toValueParam [(ConfigWindowBorderWidth, fi $ bw)]
    withConnection $ liftIO . flip configureWindow cw


insertClient :: Client -> Z ()
insertClient client = do
    setBorderWidth client
    getsL (normalBorderColor <.> config) >>= setBorderColor client
    clients <.> queue %:= (client :)


insertWindow :: ClientWindow -> Z ()
insertWindow window = do
    c <- asksL connection
    reply <- liftIO (getWindowAttributes c window >>= getReply)
    case map_state_GetWindowAttributesReply <$> reply of
        Left _ -> return ()
        Right MapStateUnviewable -> return ()
        _ -> withConnection (liftIO . flip getGeometry (convertXid window))
                >>= liftIO . getReply
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


insertWindows :: [ClientWindow] -> Z ()
insertWindows windows = do
    c <- asksL connection
    filterValid <$> attributes c
        >>= fmap (>>= makeClients) . geometries c
            >>= insert

    where
    attributes :: Connection -> Z (Either SomeError [GetWindowAttributesReply])
    attributes c = liftIO $ mapM (getWindowAttributes c) windows >>= getReplies

    geometries :: Connection -> [ClientWindow]
               -> Z (Either SomeError [(ClientWindow, GetGeometryReply)])
    geometries c ws = (Right . zip ws =<<)
        <$> liftIO (mapM (getGeometry c . convertXid) ws >>= getReplies)

    filterValid :: Either SomeError [GetWindowAttributesReply] -> [ClientWindow]
    filterValid (Right attrs) = map fst $ filter isValid $ zip windows attrs
    filterValid _ = []

    isValid :: (ClientWindow, GetWindowAttributesReply) -> Bool
    isValid = not . override_redirect_GetWindowAttributesReply . snd

    makeClients :: [(ClientWindow, GetGeometryReply)] -> Either SomeError [Client]
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

deleteWindow :: ClientWindow -> [Client] -> [Client]
deleteWindow w (c:cs)
    | w == c ^. xid = cs
    | otherwise = c : deleteWindow w cs
deleteWindow _ _ = []

removeWindow :: ClientWindow -> Z ()
removeWindow window = clients <.> queue %:= (window `deleteWindow`)

raise :: ClientWindow -> Z ()
raise window = withConnection $ liftIO . flip configureWindow cw
    where
    cw = MkConfigureWindow window mask valueparam
    mask = toMask [ConfigWindowStackMode]
    valueparam = toValueParam [(ConfigWindowStackMode, toValue StackModeAbove)]

lower :: ClientWindow -> Z ()
lower window = withConnection $ liftIO . flip configureWindow cw
    where
    cw = MkConfigureWindow window mask valueparam
    mask = toMask [ConfigWindowStackMode]
    valueparam = toValueParam [(ConfigWindowStackMode, toValue StackModeBelow)]

grabButtons :: ClientWindow -> Z ()
grabButtons window = do
    c <- asksL connection
    mask <- getsL (modMask <.> config)
    pbs <- M.keys <$> getsL (buttonPressHandler <.> config)
    rbs <- M.keys <$> getsL (buttonReleaseHandler <.> config)
    forM_ (pbs `L.union` rbs) $ \button -> do
        liftIO $ grabButton c $ MkGrabButton True window events
                                             GrabModeAsync GrabModeAsync
                                             (convertXid xidNone) (convertXid xidNone)
                                             button [mask]
        where
        events = [EventMaskButtonMotion, EventMaskButtonPress, EventMaskButtonRelease]
