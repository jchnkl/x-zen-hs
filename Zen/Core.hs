{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, TupleSections #-}


module Core where


import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import qualified Data.List as L
import Control.Monad.Free
import Control.Monad.State hiding (state)
import Control.Monad.Reader
import Control.Applicative
import Graphics.XHB
import Types
import Util
import Lens
import Log
import qualified Window as W
import qualified Keyboard as K
import qualified Model as Model
import Graphics.X11.Types (KeySym, xK_Num_Lock, xK_Caps_Lock)

{-
 TODO
 * Follow focus:
     - border color
     - warp mouse pointer when focus change by key
-}


data KeyEventHandler = KeyEventHandler
    { press   :: KeyPressEvent -> CoreStack ()
    , release :: KeyReleaseEvent -> CoreStack ()
    }
    deriving Typeable

defaultKeyEventHandler :: KeyEventHandler
defaultKeyEventHandler = KeyEventHandler (const $ return ()) (const $ return ())

data CoreConfig = CoreConfig
    { keyEventHandler :: (Map ([ModMask], KeySym) KeyEventHandler)
    }
    deriving Typeable


askConfig :: CoreStack CoreConfig
askConfig = ask


asksConfig :: (CoreConfig -> a) -> CoreStack a
-- asksConfig = lift . lift . lift . asks
asksConfig = asks


type CoreStack = ReaderT CoreConfig (Z IO)


core :: CoreConfig -> ControllerComponent
core c = Component
    { componentId = "Core"
    , componentData = c
    , execComponent = execCoreComponent
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


execCoreComponent :: CoreStack a -> CoreConfig -> Z IO CoreConfig
execCoreComponent f cc = runReaderT f cc >> return cc


startupCoreComponent :: CoreConfig -> Z IO CoreConfig
startupCoreComponent conf = do
    grabKeys conf

    mapM_ ((Model.insertClient . snd =<<) . mkClient)
        =<< filterChildren =<< children <$> rootTree

    return conf

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
        geom <- fromRight nullGeometry <$> clientGeometry w
        return . (w,) $ Client w nullPosition geom M.empty M.empty


-- handleCreateNotify :: (Typeable t, Typeable m, MonadReader CoreConfig m, MonadTrans t) => CreateNotifyEvent -> t m ()
handleCreateNotify :: CreateNotifyEvent -> CoreStack ()
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    manage (window_CreateNotifyEvent e)


handleDestroyNotify :: DestroyNotifyEvent -> CoreStack ()
handleDestroyNotify e = do
    toLog "DestroyNotifyEvent"
    unmanage (window_DestroyNotifyEvent e)


handleEnterNotify :: EnterNotifyEvent -> CoreStack ()
handleEnterNotify e = do
    lift $ whenM ((not isInferior &&) <$> Model.member window)
        $ config . focusedBorderColor $-> W.setBorderColor window
    where window = event_EnterNotifyEvent e
          isInferior = NotifyDetailInferior == detail_EnterNotifyEvent e


handleLeaveNotify :: LeaveNotifyEvent -> CoreStack ()
handleLeaveNotify e = do
    lift $ whenM ((not isInferior &&) <$> Model.member window) $ do
        config . normalBorderColor $-> W.setBorderColor window
    where window = event_LeaveNotifyEvent e
          isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e


handleKeyPress :: KeyPressEvent -> CoreStack ()
handleKeyPress e = do
    toLog . ("KeyPressEvent:\n" ++) . show $ e

    mask <- (\\) <$> (lift $ K.getCleanMask state) <*> askModMask

    mapM_ ($ e)
        =<< mapM (asksConfig . (. keyEventHandler) . lookupKeysym mask)
            =<< lift (asksL keyboardMap $ flip K.keycodeToKeysym keycode)

    where state = state_KeyPressEvent e
          keycode = detail_KeyPressEvent e
          lookupKeysym mask keysym = fromMaybe (const $ return ())
                                   . fmap press
                                   . M.lookup (mask, fi keysym)
          askModMask = lift $ askL (config . modMask)


grabKeys :: (Functor m, MonadIO m) => CoreConfig -> Z m ()
grabKeys coreconfig = rootWindow $-> \w -> do
    mapM_ (uncurry . flip $ Model.grabKey w) . M.keys . keyEventHandler $ coreconfig


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


manage :: WindowId -> CoreStack ()
manage window = lift $ whenM (isClient <$> (W.attributes window >>= reply)) $ do
    initWindow window
    Model.insert window


unmanage :: WindowId -> CoreStack ()
unmanage window = do
    Model.remove window


isUnviewable :: GetWindowAttributesReply -> Bool
isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r


isClient :: Either SomeError GetWindowAttributesReply -> Bool
isClient = fromRight False . fmap (not . isUnviewable)


