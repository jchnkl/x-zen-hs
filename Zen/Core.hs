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
import SomeState


-- type CoreST = LogWT (SetupRT (StateT Core IO))
type CoreST m a = Z (StateT Core m) a


-- class (Monad m) => MonadSetup m where
--     liftSetup :: SetupRT m a -> m a

-- instance MonadSetup CoreST where
--     liftSetup = lift

-- liftSetup :: Monad m => (SetupRT m) a -> m a
-- liftSetup = id

{-
coreState :: SomeState
coreState = Stateful (Core Normal M.empty ) coreEventHandler


coreEventHandler :: MonadIO m => SomeEvent -> CoreST m ()
coreEventHandler event = do
    toLog "coreEventHandler, state:"
    get >>= toLog . show
    mapM_ try handler
    where
    try :: EventHandler (CoreST ()) -> CoreST ()
    try (EventHandler h) = void $ whenJust (fromEvent event) h

    handler :: [EventHandler (CoreST ())]
    handler = [ EventHandler handleCreateNotify
              , EventHandler handleDestroyNotify
              ]
-}



coreState :: SomeState
coreState = Stateful (Core Normal M.empty)
                   $ eventDispatcher [ EventHandler handleCreateNotify
                                     , EventHandler handleDestroyNotify
                                     ]


handleCreateNotify :: (Functor m, MonadIO m) => CreateNotifyEvent -> CoreST m ()
handleCreateNotify = manage .  window_CreateNotifyEvent


handleDestroyNotify :: MonadIO m => DestroyNotifyEvent -> CoreST m ()
handleDestroyNotify e = toLog "DestroyNotifyEvent" >> unmanage (window_DestroyNotifyEvent e)


{-
manage :: WindowId -> CoreST ()
manage window = whenM (isClient <$> attributes) $ do
    toLog "manage"
    configure'
    queue %:= (insert $ Client window (Geometry (Position 0 0) (Dimension 0 0)) $ Position 0 0)

    where
    attributes :: CoreST (Either SomeError GetWindowAttributesReply)
    attributes = io . getReply =<<
        io . flip getWindowAttributes window <-$ connection

    isClient :: Either SomeError GetWindowAttributesReply -> Bool
    isClient (Right reply) = not $ isUnviewable reply
    isClient _             = False

    isUnviewable :: GetWindowAttributesReply -> Bool
    isUnviewable r = MapStateUnviewable == map_state_GetWindowAttributesReply r

    configure' :: CoreST ()
    configure' = do
        let mask = CWEventMask
            values = toMask [EventMaskEnterWindow, EventMaskLeaveWindow, EventMaskFocusChange]
            valueparam = toValueParam [(mask, values)]
        connection $-> \c -> io $ changeWindowAttributes c window valueparam
        -- config . borderWidth $-> setBorderWidth window
        lift $ grabButtons window


unmanage :: WindowId -> CoreST ()
unmanage w = toLog "unmanage" >> (queue %:= remove w)
-}


manage :: (Functor m, MonadIO m) => WindowId -> CoreST m ()
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
        grabButtons window


unmanage :: MonadIO m => WindowId -> CoreST m ()
unmanage w = queue %:= remove w


{-
grabButtons :: MonadIO m => WindowId -> SetupRT m ()
grabButtons window = connection $-> \c -> do
    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    eventmask <- askL buttonMask
    -- let nl = catMaybes [fmap (fromBit . toValue) $ keysymToModifier kbdmap modmap (fi xK_Num_Lock)]
    --     cl = catMaybes [fmap (fromBit . toValue) $ keysymToModifier kbdmap modmap (fi xK_Caps_Lock)]
    --     -- TODO: separate function
        -- combos m b = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [b, b ..]
    let combos m b = L.nub $ zip (m : map (m ++) []) [b, b ..]

    buttons <- asksL (config . buttonHandler) (M.keys)

    -- forM_ buttons $ \(m, b) -> mapM_ (grab c eventmask) $ combos (modmask ++ m) b
    forM_ buttons $ \(m, b) -> mapM_ (grab c eventmask) $ combos (modmask ++ m) b

    where
    grab :: MonadIO m => Connection -> [EventMask] -> ([ModMask], ButtonIndex) -> m ()
    grab c eventmask (mask, button) = do
        io $ grabButton c $ MkGrabButton True window eventmask
                            GrabModeAsync GrabModeAsync
                            (convertXid xidNone) (convertXid xidNone)
                            button mask
-}
