{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE ExistentialQuantification, StandaloneDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import qualified Data.Map as M
import qualified Data.List as L
import Data.Word
import Data.Maybe
import Data.Time (getZonedTime)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Graphics.XHB
-- import Graphics.XHB hiding (fromXid, toXid)
-- import qualified Graphics.XHB as X (fromXid, toXid)

-- TODO:
-- Lenses for data structures
-- Free Monads for Layout
-- Use Word32 for xids & convert to from WINDOW, DRAWABLE, etc.
-- Use motionNotifyEvent_root_x instead of root_x_MotionNotifyEvent

-- IDEAS
-- Use Mod4 with lock after timeout

class Accessor a where
    get :: a -> b
    set :: a -> b -> a

type ClientWindow = WINDOW

data ConnectionSetup = ConnectionSetup
    { connection :: Connection
    , root :: ClientWindow
    }

data Position = Position
    { x :: Int
    , y :: Int
    }
    deriving (Eq, Read, Show)

data Dimension = Dimension
    { width :: Word
    , height :: Word
    }
    deriving (Eq, Read, Show)

data Geometry = Geometry
    { position :: Position
    , dimension :: Dimension
    }
    deriving (Eq, Read, Show)

-- setGeometryPosition :: Geometry -> Position -> Geometry
-- setGeometryPosition g p = g { position = p }

data Client = Client
    { xid :: ClientWindow
    , geometry :: Geometry
    }
    deriving (Eq, Show)

setClientPosition :: Client -> Position -> Client
setClientPosition client p = client { geometry = (geometry client) { position = p } }

setClientDimension :: Client -> Dimension -> Client
setClientDimension client d = client { geometry = (geometry client) { dimension = d } }

-- TODO:
-- { focused :: Client
-- , stack :: [Client]
--
-- track focus! -> {Enter,Leave}NotifyEvent

data Queue = Queue
    { clients :: [Client]
    }

type ButtonPressHandler = M.Map ButtonIndex (ButtonPressEvent -> Z ())
type ButtonReleaseHandler = M.Map ButtonIndex (ButtonReleaseEvent -> Z ())

data Config = Config
    { eventHandler :: [EventHandler (Z Bool)] -- TODO: STACK!!! -> pushHandler, popHandler
    , borderWidth :: Word
    , modMask :: ModMask
    , buttonPressHandler :: ButtonPressHandler
    , buttonReleaseHandler :: ButtonReleaseHandler
    }

data Core = Core
    { config :: Config
    , queue :: Queue
    , pointer :: Position
    }

-- type Z = StateT Core (ReaderT ConnectionSetup IO)
type ZCore = StateT Core (ReaderT ConnectionSetup IO)
type Z = WriterT [String] ZCore

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

convertXid :: (XidLike a, XidLike b) => a -> b
convertXid = fromXid . toXid

unlessZ :: Z Bool -> Z () -> Z ()
unlessZ zb f = do
    b <- zb
    unless b f

whenJust :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenJust Nothing _ = return Nothing
whenJust (Just v) f = liftM Just (f v)

whenRight :: Monad m => Either a b -> (b -> m c) -> m (Maybe c)
whenRight (Left _)  _ = return Nothing
whenRight (Right v) f = liftM Just (f v)

withConnection :: (Connection -> Z a) -> Z a
withConnection f = asks connection >>= f

withRoot :: (ClientWindow -> Z a) -> Z a
withRoot f = asks root >>= f

-- withCurrentWindow :: (Client -> Z a) -> Z a
-- withCurrentWindow f = gets (current . queue) >>= f

withWindows :: ([Client] -> Z a) -> Z a
withWindows f = gets (clients . queue) >>= f

withFocused :: (Client -> Z a) -> Z a
withFocused = undefined

withClient :: ClientWindow -> (Client -> Z a) -> Z (Maybe a)
withClient w f =
    fmap (L.find ((w ==) . xid)) (gets $ clients . queue ) >>= flip whenJust f

modifyClient :: ClientWindow -> (Client -> Client) -> Z ()
modifyClient win f = modifyQueue $ \q -> q { clients = update (clients q) win f }
    where
    update :: [Client] -> ClientWindow -> (Client -> Client) -> [Client]
    update (c:cs) window fun
        | window == xid c = fun c : cs
        | otherwise = c : update cs window fun
    update _ _ _ = []

withEventHandler :: ([EventHandler (Z Bool)] -> Z ()) -> Z ()
withEventHandler f = gets (eventHandler . config) >>= f

modifyQueue :: (Queue -> Queue) -> Z ()
modifyQueue f = modify $ \core -> core { queue = f $ queue core }

modifyConfig :: (Config -> Config) -> Z ()
modifyConfig f = modify $ \core -> core { config = f $ config core }

modifyPointer :: (Position -> Position) -> Z ()
modifyPointer f = modify $ \core -> core { pointer = f $ pointer core }

putPointer :: Position -> Z ()
putPointer p = modify $ \core -> core { pointer = p }

getPointer :: Z Position
getPointer = gets pointer

insertClient :: Client -> Z ()
insertClient client = modifyQueue $ \q -> q { clients = client : clients q }

insertWindow :: ClientWindow -> Z ()
insertWindow window = do
    c <- asks connection
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
insertWindows ws = do
    c <- asks connection
    void $ (map fst . filter is_client . zip ws) <$>
        (forM ws (liftIO . getWindowAttributes c) >>= mapM (liftIO . getReply))
        >>= mapM (\w -> ((,) w) <$> liftIO (getGeometry c $ convertXid w))
            >>= fmap (map updateQueue) . mapM (\(w, r) -> ((,) w) <$> liftIO (getReply r))
        -- >>= return . windows
    -- let w' = windows attrs
    -- let windows = fmap (map fst . filter is_client . zip ws)
    -- attrs <- forM attributes $ liftIO . getReply
    return ()
    where
    is_client :: (ClientWindow, Either SomeError GetWindowAttributesReply) -> Bool
    is_client (_, Left _) = False
    is_client (_, Right attrs) = not $ override_redirect_GetWindowAttributesReply attrs

    updateQueue (_, Left _) = return ()
    updateQueue (window, Right geom) = do
        let x' = fi $ x_GetGeometryReply geom
            y' = fi $ y_GetGeometryReply geom
            w' = fi $ width_GetGeometryReply geom
            h' = fi $ height_GetGeometryReply geom
            c' = Client window (Geometry (Position x' y') (Dimension w' h'))
        insertClient c'

removeClient :: Client -> Z ()
removeClient client = modifyQueue $ \q -> q { clients = client `L.delete` clients q }

deleteWindow :: ClientWindow -> [Client] -> [Client]
deleteWindow w (c:cs)
    | w == xid c = cs
    | otherwise = c : deleteWindow w cs
deleteWindow _ _ = []

removeWindow :: ClientWindow -> Z ()
removeWindow window = modifyQueue $ \q -> Queue $ window `deleteWindow` (clients q)

instance Eq ButtonIndex where
    l == r = (toValue l :: Word8) == (toValue r :: Word8)

instance Ord ButtonIndex where
    compare l r = compare (toValue l :: Word8) (toValue r :: Word8)

moveWindow :: MotionNotifyEvent -> Z Bool
moveWindow e = do
    withConnection $ \c -> gets pointer >>= configure c window
    return True
    where
    root_x = fi $ root_x_MotionNotifyEvent e
    root_y = fi $ root_y_MotionNotifyEvent e
    window = event_MotionNotifyEvent e
    configure c w (Position x' y') = liftIO $ configureWindow c $
        MkConfigureWindow w (toMask [ConfigWindowX, ConfigWindowY])
                            (toValueParam [(ConfigWindowX, root_x - fi x'),
                                           (ConfigWindowY, root_y - fi y')])

resizeWindow :: MotionNotifyEvent -> Z Bool
resizeWindow e = do
    toLog "resizeWindow"
    -- TODO: do this with focused client
    void $ withConnection $ withClient window . configure

    return True

    where
    window = event_MotionNotifyEvent e
    newx = fi (root_x_MotionNotifyEvent e)
    newy = fi (root_y_MotionNotifyEvent e)
    -- w' x' g = x' (fi (root_x_MotionNotifyEvent e) - fi x')
    -- h' y' g = y' (fi (root_y_MotionNotifyEvent e) - fi y')

    -- configure _ Nothing = return ()
    configure c (Client _ (Geometry _ (Dimension w' h'))) = do
        Position oldx oldy <- getPointer
        let neww = fi w' + fi (newx - oldx)
            newh = fi h' + fi (newy - oldy)
        -- return ()
        toLog $ "resize to " ++ show neww ++ "x" ++ show newh
        liftIO $ configureWindow c $
            MkConfigureWindow window
                              (toMask [ConfigWindowWidth, ConfigWindowHeight])
                              (toValueParam [(ConfigWindowWidth, neww),
                                             (ConfigWindowHeight, newh)])
        -- putPointer $ Position newx newy

    -- configure _ _ (Left _) = return ()
    -- configure c (Position x' y') (Right g) = do
    --     toLog $ "resize to " ++ show (w' x' g :: Int) ++ "x" ++ show (h' y' g :: Int)
    --     liftIO $ configureWindow c $
    --         MkConfigureWindow window
    --                           (toMask [ConfigWindowWidth, ConfigWindowHeight])
    --                           (toValueParam [(ConfigWindowWidth, w' x' g),
    --                                          (ConfigWindowHeight, h' y' g)])

defaultButtonPressHandler :: ButtonPressHandler
defaultButtonPressHandler = M.fromList
    [ (ButtonIndex1, \e -> do
            toLog "Press ButtonIndex1"
            let window = event_ButtonPressEvent e
                event_x = event_x_ButtonPressEvent e
                event_y = event_y_ButtonPressEvent e

            raise window
            modifyPointer $ const $ Position (fi event_x) (fi event_y)
            modifyConfig $ \conf ->
                conf { eventHandler = EventHandler moveWindow : eventHandler conf }
      )

    , (ButtonIndex3, \e -> do
            toLog "Press ButtonIndex2"
            let window = event_ButtonPressEvent e
            lower window
      )

    , (ButtonIndex2, \e -> do
            toLog "Press ButtonIndex3"
            let window = event_ButtonPressEvent e
                root_x = root_x_ButtonPressEvent e
                root_y = root_y_ButtonPressEvent e
                w' = fi . width_GetGeometryReply
                h' = fi . height_GetGeometryReply
                update g = modifyClient window $ flip setClientDimension (Dimension (w' g) (h' g))

            raise window
            -- TODO: do this with event_{x,y} and save pointer position in client
            modifyPointer $ const $ Position (fi root_x) (fi root_y)
            void $ flip whenRight update =<< liftIO . getReply =<<
                withConnection (liftIO . flip getGeometry (convertXid window))
            modifyConfig $ \conf ->
                conf { eventHandler = EventHandler resizeWindow : eventHandler conf }
      )
    ]

defaultButtonReleaseHandler :: ButtonReleaseHandler
defaultButtonReleaseHandler = M.fromList
    [ (ButtonIndex1, const $ do
        toLog "Release ButtonIndex1"
        modifyConfig $ \conf ->
            conf { eventHandler = drop 1 $ eventHandler conf })
    , (ButtonIndex3, const $ do
        toLog "Release ButtonIndex2")
        -- modifyConfig $ \conf ->
        --     conf { eventHandler = drop 1 $ eventHandler conf })
    , (ButtonIndex2, const $ do
        toLog "Release ButtonIndex3"
        modifyConfig $ \conf ->
            conf { eventHandler = drop 1 $ eventHandler conf })
    ]

main :: IO ()
main = connect >>= runLoop

toLog :: String -> Z ()
toLog s = tell [s]

runLoop :: Maybe Connection -> IO ()
runLoop Nothing = print "Got no connection!"
runLoop (Just c) = runReaderT (evalStateT runZ initialCore) initialSetup
    where
    initialCore = Core initialConfig initialWindowQueue (Position 0 0)
    initialSetup = ConnectionSetup c (getRoot c)
    initialWindowQueue = Queue []
    handler = [ EventHandler handleCreateNotify
              , EventHandler handleDestroyNotify
              , EventHandler handleMapRequest
              , EventHandler handleConfigureRequest
              , EventHandler handleButtonPress
              , EventHandler handleButtonRelease
              ]
    initialConfig = Config
        { eventHandler = handler
        , borderWidth = 3
        , modMask = ModMask1
        , buttonPressHandler = defaultButtonPressHandler
        , buttonReleaseHandler = defaultButtonReleaseHandler
        }

runZ :: ZCore ()
runZ = do
    c <- asks connection

    let mask = CWEventMask
        values = toMask [EventMaskSubstructureRedirect, EventMaskSubstructureNotify]
        valueparam = toValueParam [(mask, values)]
    liftIO $ changeWindowAttributes c (getRoot c) valueparam

    void $ execWriterT $ withRoot (liftIO . queryTree c) >>=
                            lift . liftIO . getReply >>= manage

    -- Main loop
    forever $ do
        time <- fmap show $ liftIO getZonedTime
        liftIO . putStrLn . (show time ++) . ("\n" ++) . unlines . map ("\t" ++)
            =<< execWriterT (liftIO (waitForEvent c) >>= dispatch)

    where
    manage (Left _) = return ()
    manage (Right tree) = do
        mapM_ grabButtons (children_QueryTreeReply tree)
        mapM_ insertWindow (children_QueryTreeReply tree)

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
    c <- asks connection
    mask <- gets (modMask . config)
    pbs <- gets (M.keys . buttonPressHandler . config)
    rbs <- gets (M.keys . buttonReleaseHandler . config)
    forM_ (pbs `L.union` rbs) $ \button -> do
        liftIO $ grabButton c $ MkGrabButton True window events
                                             GrabModeAsync GrabModeAsync
                                             (convertXid xidNone) (convertXid xidNone)
                                             button [mask]
        where
        events = [EventMaskButtonMotion, EventMaskButtonPress, EventMaskButtonRelease]

data EventHandler b = forall a . Event a => EventHandler (a -> b)

-- deriving instance Eq (EventHandler a) => Eq a

dispatch :: SomeEvent -> Z ()
dispatch e = withEventHandler runHandler
    where
    runHandler :: [EventHandler (Z Bool)] -> Z ()
    runHandler (EventHandler h:hs) = do
        taken <- fromMaybe (return False) $ liftM h $ fromEvent e
        unless taken $ runHandler hs
    runHandler _ = return ()

handleCreateNotify :: CreateNotifyEvent -> Z Bool
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    insertWindow window
    grabButtons window
    return True
    where window = window_CreateNotifyEvent e

handleDestroyNotify :: DestroyNotifyEvent -> Z Bool
handleDestroyNotify e = do
    toLog "DestroyNotifyEvent"
    removeWindow (window_DestroyNotifyEvent e) >> return True

handleMapRequest :: MapRequestEvent -> Z Bool
handleMapRequest e = do
    toLog "MapRequestEvent"
    liftIO $ print e
    withConnection $ liftIO . flip mapWindow event_window
    -- grabButtons event_window
    return True
    where
    event_window = window_MapRequestEvent e

handleConfigureRequest :: ConfigureRequestEvent -> Z Bool
handleConfigureRequest e = do
    toLog "ConfigureRequestEvent"
    c <- asks connection
    liftIO $ do
        print e
        configureWindow c $ MkConfigureWindow win mask values
    liftIO (pollForError c) >>= handleError
    return True
    where
    win = window_ConfigureRequestEvent e
    mask = toMask $ value_mask_ConfigureRequestEvent e
    values = toValueParam $ copyValues (value_mask_ConfigureRequestEvent e)

    copyValues :: [ConfigWindow] -> [(ConfigWindow, Word32)]
    copyValues (ConfigWindowX           : ms) = (ConfigWindowX,           fi $ x_ConfigureRequestEvent e)                    : copyValues ms
    copyValues (ConfigWindowY           : ms) = (ConfigWindowY,           fi $ y_ConfigureRequestEvent e)                    : copyValues ms
    copyValues (ConfigWindowWidth       : ms) = (ConfigWindowWidth,       fi $ width_ConfigureRequestEvent e)                : copyValues ms
    copyValues (ConfigWindowHeight      : ms) = (ConfigWindowHeight,      fi $ height_ConfigureRequestEvent e)               : copyValues ms
    copyValues (ConfigWindowBorderWidth : ms) = (ConfigWindowBorderWidth, fi $ border_width_ConfigureRequestEvent e)         : copyValues ms
    copyValues (ConfigWindowSibling     : ms) = (ConfigWindowSibling,     convertXid $ sibling_ConfigureRequestEvent e) : copyValues ms
    copyValues (ConfigWindowStackMode   : ms) = (ConfigWindowStackMode,   toValue $ stack_mode_ConfigureRequestEvent e)      : copyValues ms
    copyValues _ = []


handleButtonPress :: ButtonPressEvent -> Z Bool
handleButtonPress e = do
    toLog "ButtonPressEvent"
    mask <- gets (modMask . config)
    if toBit mask `notElem` map toBit (state_ButtonPressEvent e)
        then return False
        else do
            fs <- gets (buttonPressHandler . config)
            void $ whenJust (M.lookup (fromValue $ detail_ButtonPressEvent e) fs) $ \f -> f e
            return True

handleButtonRelease :: ButtonReleaseEvent -> Z Bool
handleButtonRelease e = do
    toLog "ButtonReleaseEvent"
    mask <- gets (modMask . config)
    if toBit mask `notElem` map toBit (state_ButtonReleaseEvent e)
        then return False
        else do
            fs <- gets (buttonReleaseHandler . config)
            void $ whenJust (M.lookup (fromValue $ detail_ButtonReleaseEvent e) fs) $ \f -> f e
            return True


handleError :: Maybe SomeError -> Z ()
handleError Nothing = return ()
handleError (Just se) = toLog $ "ERROR: " ++ show se
