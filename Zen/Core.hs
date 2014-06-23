{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, TupleSections #-}


module Core where


import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow (second)
import Control.Applicative
import Graphics.XHB
import Types
import Util
import Lens
import Log
import Message
import qualified Queue as Q
import qualified Window as W
import qualified Keyboard as K
import Graphics.X11.Types (KeySym, xK_Num_Lock, xK_Caps_Lock)

{-
 TODO
 * Follow focus:
     - border color
     - warp mouse pointer when focus change by key
-}


data Mode = Normal | Manage
    deriving (Eq, Read, Show, Typeable)

data KeyEventHandler = KeyEventHandler
    { press   :: KeyPressEvent -> Z CoreState ()
    , release :: KeyReleaseEvent -> Z CoreState ()
    }
    deriving Typeable

defaultKeyEventHandler :: KeyEventHandler
defaultKeyEventHandler = KeyEventHandler (const $ return ()) (const $ return ())

data CoreConfig = CoreConfig
    { _keyEventHandler :: (Map ([ModMask], KeySym) KeyEventHandler)
    }
    deriving Typeable

keyEventHandler :: Functor f => LensLike' f CoreConfig (Map ([ModMask], KeySym) KeyEventHandler)
keyEventHandler = lens _keyEventHandler (\d v -> d { _keyEventHandler = v })


data Core = Core
    { _queue :: Queue
    }
    deriving Typeable


queue :: Functor f => LensLike' f Core Queue
queue = lens _queue (\d v -> d { _queue = v })


asksCC :: (CoreConfig -> a) -> Z CoreState a
asksCC = lift . lift . lift . asks


type CoreState = ReaderT CoreConfig (StateT Core IO)


core :: CoreConfig -> Component
core c = Component
    { componentData = (Core (ClientQueue [] Nothing []), c)
    , ioRunComponent = runCoreComponent
    , onStartup = startupCoreComponent
    , onShutdown = const $ return ()
    , someHandler = const $
        map SomeHandler [ EventHandler handleCreateNotify
                        , EventHandler handleDestroyNotify
                        , EventHandler handleKeyPress
                        , EventHandler handleEnterNotify
                        , EventHandler handleLeaveNotify
                        ]
    }


runCoreComponent :: CoreState a -> (Core, CoreConfig) -> IO (a, (Core, CoreConfig))
runCoreComponent f (c, cc) = second (,cc) <$> runStateT (runReaderT f cc) c


startupCoreComponent :: (Core, CoreConfig) -> Z IO (Core, CoreConfig)
startupCoreComponent (core, config) = do
    grabKeys config
    (,config) <$> (core &) . (queue .~) . Q.fromList <$>
        (mapM mkClient =<< filterChildren =<< children <$> rootTree)

    where
    children :: Either SomeError QueryTreeReply -> [WindowId]
    children = fromRight [] . fmap children_QueryTreeReply

    rootTree :: (MonadIO m, Functor m) => Z m (Either SomeError QueryTreeReply)
    rootTree = connection $-> \c -> io (queryTree c $ getRoot c) >>= reply

    filterChildren :: (MonadIO m, Functor m) => [WindowId] -> Z m [WindowId]
    filterChildren = filterM ((isClient <$>) . (reply =<<) . W.attributes)

    clientGeometry :: (MonadIO m, Functor m) => WindowId -> Z m (Either SomeError Geometry)
    clientGeometry w = fmap convert <$> (W.geometry w >>= reply)

    mkClient :: (MonadIO m, Functor m) => WindowId -> Z m (WindowId, Client)
    mkClient w = do
        initWindow w
        (w,) . Client w nullPosition . fromRight nullGeometry <$> clientGeometry w


handleCreateNotify :: CreateNotifyEvent -> Z CoreState ()
handleCreateNotify e = toLog "Core CreateNotifyEvent" >> manage (window_CreateNotifyEvent e)


handleDestroyNotify :: DestroyNotifyEvent -> Z CoreState ()
handleDestroyNotify e = toLog "Core DestroyNotifyEvent" >> unmanage (window_DestroyNotifyEvent e)


handleEnterNotify :: EnterNotifyEvent -> Z CoreState ()
handleEnterNotify e = whenM (getsL queue $ (not isInferior &&) . Q.member window)
    $ config . focusedBorderColor $-> W.setBorderColor window
    where window = event_EnterNotifyEvent e
          isInferior = NotifyDetailInferior == detail_EnterNotifyEvent e


handleLeaveNotify :: LeaveNotifyEvent -> Z CoreState ()
handleLeaveNotify e = whenM (getsL queue $ (not isInferior &&) . Q.member window)
    $ config . normalBorderColor $-> W.setBorderColor window
    where window = event_LeaveNotifyEvent e
          isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e


handleKeyPress :: KeyPressEvent -> Z CoreState ()
handleKeyPress e = do
    toLog . ("KeyPressEvent:\n" ++) . show $ e
    let state = state_KeyPressEvent e
        keycode = detail_KeyPressEvent e

    mask <- (\\) <$> (K.getCleanMask state) <*> askL (config . modMask)

    let lookupKeysym keysym = fromMaybe (const $ return ())
                            . fmap press
                            . M.lookup (mask, fi keysym)
    mapM_ (\f -> f e)
        =<< (mapM $ getsL (coreConfig . keyEventHandler) . lookupKeysym)
            =<< asksL keyboardMap (flip K.keycodeToKeysym keycode)


grabKeys :: (Functor m, MonadIO m) => CoreConfig -> Z m ()
grabKeys coreconfig = connection $-> \c -> do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    modmask <- askL (config . modMask)
    let keys = M.keys $ coreconfig ^. keyEventHandler
    -- keys <- getsL (core ^. coreConfig . keyEventHandler) M.keys

    let nl = catMaybes [(fromBit . toValue) <$> K.keysymToModifier kbdmap modmap (fi xK_Num_Lock)]
        cl = catMaybes [(fromBit . toValue) <$> K.keysymToModifier kbdmap modmap (fi xK_Caps_Lock)]
        -- TODO: separate function
        combos m kc = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]
        grab (mask, keycode) = io $ grabKey c $ MkGrabKey True (getRoot c)
                                                          mask keycode
                                                          GrabModeAsync GrabModeAsync

    forM_ keys $ \(mask, keysym) -> do
        toLog . ("grabbing " ++) . show $ (mask, keysym)
        whenJustM_ (K.keysymToKeycode kbdmap (fi keysym)) $
            mapM_ grab . combos (modmask ++ mask)


initWindow :: MonadIO m => WindowId -> Z m ()
initWindow window = do
    W.changeAttributes window [(CWEventMask, values)]
    config . borderWidth $-> W.setBorderWidth window
    config . normalBorderColor $-> W.setBorderColor window
    where
    values = toMask [ EventMaskEnterWindow
                    , EventMaskLeaveWindow
                    , EventMaskFocusChange
                    ]


manage :: WindowId -> Z CoreState ()
manage window = whenM (isClient <$> (W.attributes window >>= reply)) $ do
    initWindow window
    queue %:= Q.insert (Client window nullPosition nullGeometry)


unmanage :: WindowId -> Z CoreState ()
unmanage w = queue %:= Q.remove w


refresh :: Z CoreState ()
refresh = getL queue >>= refreshBorders
    where
    refreshBorders (ClientQueue as mc bs) = do
        whenJustM_ mc $ \c -> do
            W.focus (c ^. xid)
            refreshBorder focusedBorderColor c
        forM_ (as ++ bs) $ refreshBorder normalBorderColor

    refreshBorder bc c = config . bc $-> W.setBorderColor (c ^. xid)


isUnviewable :: GetWindowAttributesReply -> Bool
isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r


isClient :: Either SomeError GetWindowAttributesReply -> Bool
isClient = fromRight False . fmap (not . isUnviewable)


modifyQueue :: (Functor m, MonadIO m) => (Queue -> Queue) -> Z m ()
modifyQueue f = sendMessage_ (ModifyQueue f)


withQueue :: (Functor m, MonadIO m) => (Queue -> a) -> Z m a
withQueue f = f . fromMaybe Q.empty . fmap getQueueReply <$> sendMessage GetQueue


withQueueM :: (Functor m, MonadIO m) => (Queue -> Z m a) -> Z m a
withQueueM f = f =<< fromMaybe Q.empty . fmap getQueueReply <$> sendMessage GetQueue


withClient :: (Functor m, MonadIO m) => WindowId -> (Client -> a) -> Z m (Maybe a)
withClient w f = (fmap f . getClientReply =<<) <$> sendMessage (GetClient w)


withClientM :: (Functor m, MonadIO m) => WindowId -> (Client -> Z m a) -> Z m (Maybe a)
withClientM w f = flip whenJustM f
                      =<< (getClientReply =<<) <$> sendMessage (GetClient w)


handleCoreMessages :: MessageCom -> Z CoreState ()
handleCoreMessages = sendReply handle
    where
    handle (IsClient window)  = IsClientReply <$> getsL queue (Q.member window)

    handle (GetClient window) = GetClientReply <$> getsL queue (Q.lookup window)

    handle (GetQueue)         = GetQueueReply <$> getL queue

    handle (WithClient w f)   = WithClientReply <$> getsL queue (fmap f . Q.lookup w)

    handle (WithQueue f)      = WithQueueReply <$> getsL queue f

    handle (ModifyClient w f) = queue %:= Q.modifyClient w f >> return VoidReply

    handle (ModifyQueue f)    = queue %:= f >> return VoidReply
