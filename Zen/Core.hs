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
import Queue
import Window


type CoreState = StateT Core IO
coreComponent :: Component
coreComponent = Component
    { componentData = Core Normal M.empty
    , runComponent = runCoreComponent
    , onStartup = return . id
    , onShutdown = const $ return ()
    , someSinks = const $ [ EventHandler handleCreateNotify
                          , EventHandler handleDestroyNotify
                          , MessageHandler handleCoreMessages
                          ]
    }


runCoreComponent :: CoreState a -> Core -> IO (a, Core)
runCoreComponent = runStateT


handleCreateNotify :: CreateNotifyEvent -> Z CoreState ()
handleCreateNotify e = toLog "Core CreateNotifyEvent" >> manage (window_CreateNotifyEvent e)


handleDestroyNotify :: DestroyNotifyEvent -> Z CoreState ()
handleDestroyNotify e = toLog "Core DestroyNotifyEvent" >> unmanage (window_DestroyNotifyEvent e)


manage :: WindowId -> Z CoreState ()
manage window = whenM (isClient <$> attributes) $ do
    configure'
    queue %:= (insert $ Client window (Geometry (Position 0 0) (Dimension 0 0)) $ Position 0 0)

    where
    attributes :: MonadIO m => Z m (Either SomeError GetWindowAttributesReply)
    attributes = io . getReply =<<
        io . flip getWindowAttributes window <-$ connection

    configure' :: MonadIO m => Z m ()
    configure' = do
        let mask = CWEventMask
            values = toMask [EventMaskEnterWindow, EventMaskLeaveWindow, EventMaskFocusChange]
            valueparam = toValueParam [(mask, values)]
        connection $-> \c -> io $ changeWindowAttributes c window valueparam
        config . borderWidth $-> setBorderWidth window
        -- grabButtons window


unmanage :: WindowId -> Z CoreState ()
unmanage w = queue %:= remove w



isUnviewable :: GetWindowAttributesReply -> Bool
isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r

isClient :: Either SomeError GetWindowAttributesReply -> Bool
isClient = fromRight False . fmap (not . isUnviewable)
