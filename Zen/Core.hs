{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, TupleSections #-}


module Core where


import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\))
import qualified Data.List as L
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
    { press   :: KeyPressEvent -> Z CoreState ()
    , release :: KeyReleaseEvent -> Z CoreState ()
    }
    deriving Typeable

defaultKeyEventHandler :: KeyEventHandler
defaultKeyEventHandler = KeyEventHandler (const $ return ()) (const $ return ())

data CoreConfig = CoreConfig
    { keyEventHandler :: (Map ([ModMask], KeySym) KeyEventHandler)
    }
    deriving Typeable


askConfig :: Z CoreState CoreConfig
askConfig = lift . lift . lift $ ask


asksConfig :: (CoreConfig -> a) -> Z CoreState a
asksConfig = lift . lift . lift . asks


type CoreState = ReaderT CoreConfig IO


core :: CoreConfig -> Component
core c = Component
    { componentId = "Core"
    , componentData = c
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


runCoreComponent :: CoreState a -> CoreConfig -> IO (a, CoreConfig)
runCoreComponent f cc = (,cc) <$> runReaderT f cc


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
        (w,) . Client w nullPosition . fromRight nullGeometry <$> clientGeometry w


handleCreateNotify :: CreateNotifyEvent -> Z CoreState ()
handleCreateNotify e = do
    toLog "CreateNotifyEvent"
    manage (window_CreateNotifyEvent e)


handleDestroyNotify :: DestroyNotifyEvent -> Z CoreState ()
handleDestroyNotify e = do
    toLog "DestroyNotifyEvent"
    unmanage (window_DestroyNotifyEvent e)


handleEnterNotify :: EnterNotifyEvent -> Z CoreState ()
handleEnterNotify e = do
    whenM ((not isInferior &&) <$> Model.member window)
        $ config . focusedBorderColor $-> W.setBorderColor window
    where window = event_EnterNotifyEvent e
          isInferior = NotifyDetailInferior == detail_EnterNotifyEvent e


handleLeaveNotify :: LeaveNotifyEvent -> Z CoreState ()
handleLeaveNotify e = do
    whenM ((not isInferior &&) <$> Model.member window) $ do
        config . normalBorderColor $-> W.setBorderColor window
    where window = event_LeaveNotifyEvent e
          isInferior = NotifyDetailInferior == detail_LeaveNotifyEvent e


handleKeyPress :: KeyPressEvent -> Z CoreState ()
handleKeyPress e = do
    toLog . ("KeyPressEvent:\n" ++) . show $ e

    mask <- (\\) <$> (K.getCleanMask state) <*> askL (config . modMask)

    mapM_ ($ e)
        =<< mapM (asksConfig . (. keyEventHandler) . lookupKeysym mask)
            =<< asksL keyboardMap (flip K.keycodeToKeysym keycode)

    where state = state_KeyPressEvent e
          keycode = detail_KeyPressEvent e
          lookupKeysym mask keysym = fromMaybe (const $ return ())
                                   . fmap press
                                   . M.lookup (mask, fi keysym)


grabKeys :: (Functor m, MonadIO m) => CoreConfig -> Z m ()
grabKeys coreconfig = connection $-> \c -> do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    modmask <- askL (config . modMask)

    let keys = M.keys $ keyEventHandler coreconfig
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
    Model.insert window


unmanage :: WindowId -> Z CoreState ()
unmanage window = do
    Model.remove window


isUnviewable :: GetWindowAttributesReply -> Bool
isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r


isClient :: Either SomeError GetWindowAttributesReply -> Bool
isClient = fromRight False . fmap (not . isUnviewable)


