{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}


module Core where


import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative
import Graphics.XHB
import Types
import Util
import Lens
import Log
import Message
import qualified Queue as Q
import qualified Window as W

{-
 TODO
 * Follow focus:
     - border color
     - warp mouse pointer when focus change by key
-}

type CoreState = StateT Core IO


coreComponent :: Component
coreComponent = Component
    { componentData = Core Normal M.empty
    , runComponent = runCoreComponent
    , onStartup = startupCoreComponent
    , onShutdown = const $ return ()
    , someSinks = const $ [ EventHandler handleCreateNotify
                          , EventHandler handleDestroyNotify
                          , EventHandler handleFocusIn
                          , MessageHandler handleCoreMessages
                          ]
    }


runCoreComponent :: CoreState a -> Core -> IO (a, Core)
runCoreComponent = runStateT


startupCoreComponent :: Core -> Z IO Core
startupCoreComponent core = do
    (core &) . (queue .~) . M.fromList <$>
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
        (w,) . Client w . fromRight nullGeometry <$> clientGeometry w


handleCreateNotify :: CreateNotifyEvent -> Z CoreState ()
handleCreateNotify e = toLog "Core CreateNotifyEvent" >> manage (window_CreateNotifyEvent e)


handleDestroyNotify :: DestroyNotifyEvent -> Z CoreState ()
handleDestroyNotify e = toLog "Core DestroyNotifyEvent" >> unmanage (window_DestroyNotifyEvent e)

handleFocusIn :: FocusInEvent -> Z CoreState ()
handleFocusIn = toLog . ("FocusInEvent " ++) . show


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
    queue %:= (Q.insert $ Client window (Geometry (Position 0 0) (Dimension 0 0)))


unmanage :: WindowId -> Z CoreState ()
unmanage w = queue %:= Q.remove w


isUnviewable :: GetWindowAttributesReply -> Bool
isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r


isClient :: Either SomeError GetWindowAttributesReply -> Bool
isClient = fromRight False . fmap (not . isUnviewable)


modifyQueue :: (Functor m, MonadIO m) => (Queue -> Queue) -> Z m ()
modifyQueue f = sendMessage_ (ModifyQueue f)


withQueue :: (Functor m, MonadIO m) => (Queue -> a) -> Z m a
withQueue f = f . fromMaybe M.empty . fmap getQueueReply <$> sendMessage GetQueue


withQueueM :: (Functor m, MonadIO m) => (Queue -> Z m a) -> Z m a
withQueueM f = f =<< fromMaybe M.empty . fmap getQueueReply <$> sendMessage GetQueue


withClient :: (Functor m, MonadIO m) => WindowId -> (Client -> a) -> Z m (Maybe a)
withClient w f = (fmap f . getClientReply =<<) <$> sendMessage (GetClient w)


withClientM :: (Functor m, MonadIO m) => WindowId -> (Client -> Z m a) -> Z m (Maybe a)
withClientM w f = flip whenJustM f
                      =<< (getClientReply =<<) <$> sendMessage (GetClient w)


handleCoreMessages :: MessageCom -> Z CoreState ()
handleCoreMessages = sendReply handle
    where
    handle (IsClient window)  = IsClientReply <$> getsL queue (M.member window)

    handle (GetClient window) = GetClientReply <$> getsL queue (M.lookup window)

    handle (GetQueue)         = GetQueueReply <$> getL queue

    handle (WithClient w f)   = WithClientReply <$> getsL queue (fmap f . M.lookup w)

    handle (WithQueue f)      = WithQueueReply <$> getsL queue f

    handle (ModifyClient w f) = queue %:= Q.modifyClient w f >> return VoidReply

    handle (ModifyQueue f)    = queue %:= f >> return VoidReply
