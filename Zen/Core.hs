{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}


module Core where


import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative
import Graphics.XHB
import Types
import Util
import Lens
import Log
import Queue
import Message
import qualified Window as W
-- import Keyboard


-- data Core = Core
--     { _mode :: Mode
--     , _queue :: Queue
--     }
--     deriving (Show, Typeable)


type CoreState = StateT Core IO


coreComponent :: Component
coreComponent = Component
    { componentData = Core Normal M.empty
    , runComponent = runCoreComponent
    -- , onStartup = return . id
    , onStartup = startupCoreComponent
    , onShutdown = const $ return ()
    , someSinks = [ EventHandler handleCreateNotify
                  , EventHandler handleDestroyNotify
                  , MessageHandler handleCoreMessages
                  ]
    }


runCoreComponent :: CoreState a -> Core -> IO (a, Core)
runCoreComponent = runStateT


startupCoreComponent :: Core -> Z IO Core
startupCoreComponent core = do
    toLog "startupCoreComponent"
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
    mkClient w = (w,) . Client w . fromRight nullGeometry <$> clientGeometry w


handleCreateNotify :: CreateNotifyEvent -> Z CoreState ()
handleCreateNotify e = toLog "Core CreateNotifyEvent" >> manage (window_CreateNotifyEvent e)


handleDestroyNotify :: DestroyNotifyEvent -> Z CoreState ()
handleDestroyNotify e = toLog "Core DestroyNotifyEvent" >> unmanage (window_DestroyNotifyEvent e)


manage :: WindowId -> Z CoreState ()
manage window = whenM (isClient <$> (W.attributes window >>= reply)) $ do
    configure'
    queue %:= (insert $ Client window (Geometry (Position 0 0) (Dimension 0 0)))

    where

    configure' :: MonadIO m => Z m ()
    configure' = do
        let mask = CWEventMask
            values = toMask [EventMaskEnterWindow, EventMaskLeaveWindow, EventMaskFocusChange]
            valueparam = toValueParam [(mask, values)]
        connection $-> \c -> io $ changeWindowAttributes c window valueparam
        config . borderWidth $-> W.setBorderWidth window
        -- grabButtons window


unmanage :: WindowId -> Z CoreState ()
unmanage w = queue %:= remove w



isUnviewable :: GetWindowAttributesReply -> Bool
isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r


isClient :: Either SomeError GetWindowAttributesReply -> Bool
isClient = fromRight False . fmap (not . isUnviewable)


handleCoreMessages :: MessageCom -> Z CoreState ()
handleCoreMessages = sendReply handle
    where
    handle (IsClient window) = IsClientReply <$> getsL queue (M.member window)
    handle GetClients        = GetClientsReply <$> windows
    windows = getsL queue $ map (^. xid) . M.elems
