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
import Component


type CoreState = StateT Core IO
coreComponent :: Component
coreComponent = Component
    { component = Core Normal M.empty
    , runComponent = runCoreComponent
    , initialize = return ()
    , terminate = return ()
    , handleEvent = eventDispatcher [ EventHandler handleCreateNotify
                                    , EventHandler handleDestroyNotify
                                    ]
    , handleMessage = (\_ -> return ())
    }


runCoreComponent :: CoreState a
                 -> Core
                 -> IO (a, Core)
runCoreComponent = runStateT


handleCreateNotify :: CreateNotifyEvent -> Z CoreState ()
handleCreateNotify = manage .  window_CreateNotifyEvent


handleDestroyNotify :: DestroyNotifyEvent -> Z CoreState ()
handleDestroyNotify e = toLog "DestroyNotifyEvent" >> unmanage (window_DestroyNotifyEvent e)


manage :: WindowId -> Z CoreState ()
manage window = whenM (isClient <$> attributes) $ do
    configure'
    queue %:= (insert $ Client window (Geometry (Position 0 0) (Dimension 0 0)) $ Position 0 0)

    where
    attributes :: MonadIO m => Z m (Either SomeError GetWindowAttributesReply)
    attributes = io . getReply =<<
        io . flip getWindowAttributes window <-$ connection

    isClient :: Either SomeError GetWindowAttributesReply -> Bool
    isClient (Right reply) = not $ isUnviewable reply
    isClient _             = False

    isUnviewable :: GetWindowAttributesReply -> Bool
    isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r

    configure' :: MonadIO m => Z m ()
    configure' = do
        let mask = CWEventMask
            values = toMask [EventMaskEnterWindow, EventMaskLeaveWindow, EventMaskFocusChange]
            valueparam = toValueParam [(mask, values)]
        connection $-> \c -> io $ changeWindowAttributes c window valueparam
        config . borderWidth $-> setBorderWidth window


unmanage :: WindowId -> Z CoreState ()
unmanage w = queue %:= remove w
