{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Core where


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


type CoreState = StateT Core IO


coreComponent :: Component
coreComponent = Component
    { componentData = Core Normal M.empty
    , runComponent = runCoreComponent
    , onStartup = startupCoreComponent
    , onShutdown = const $ return ()
    , someSinks = const $ [ EventHandler handleCreateNotify
                          , EventHandler handleDestroyNotify
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
