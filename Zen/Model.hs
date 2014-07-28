-- vim:sw=4:sts=4:ts=4

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TemplateHaskell  #-}

module Model where


import Data.Word
import Data.Maybe (isJust)
import Data.Map as M
import Data.Set as S
import Data.List as L
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Trans.Free

import Log
import Types
import Lens hiding (views)
import qualified Queue as Q

{-
 TODO
 * Follow focus:
     - border color
     - warp mouse pointer when focus change by key
-}


updateModel :: (MonadState Model m) => ModelOps t -> m ()
updateModel = \case
    GetQueue _           -> return () -- getL queue >>= lift . f
    PutQueue _ _         -> return () -- putL queue q >> lift f
    InsertClient c _     -> insertc c
    RemoveClient c _     -> removec c
    InsertWindow w _     -> insertw w
    RemoveWindow w _     -> removew w
    GrabKey w ks mm _    -> modc w $ keyGrabs %~ M.alter (updateMask mm) ks
    GrabButton w bi mm _ -> modc w $ buttonGrabs %~ M.alter (updateMask mm) bi
    Raise _ _            -> return ()
    Lower _ _            -> return ()
    SetX w v _           -> modc w (geometry . position . x .~ v)
    SetY w v _           -> modc w (geometry . position . y .~ v)
    SetWidth  w v _      -> modc w (geometry . dimension . width  .~ v)
    SetHeight w v _      -> modc w (geometry . dimension . height .~ v)
    SetPosition  w p _   -> modc w (geometry . position .~ p)
    SetDimension w d _   -> modc w (geometry . dimension .~ d)
    SetGeometry  w g _   -> modc w (geometry .~ g)
    SetBorderColor _ _ _ -> return ()
    SetBorderWidth _ _ _ -> return ()

    where modc w f = modify $ queue %~ Q.modifyClient w f
          removec = removew . (^. xid)
          insertc = modify . (queue %~) . Q.insert
          removew = modify . (queue %~) . Q.remove
          insertw w = insertc (Client w nullPosition nullGeometry M.empty M.empty)
          updateMask m m' = if isJust m' then fmap (m `L.union`) m' else Just m


getQueue :: (MonadFree ModelOps m) => m Queue
getQueue = liftF (GetQueue id)


putQueue :: (MonadFree ModelOps m) => Queue -> m ()
putQueue q = liftF (PutQueue q ())


insertClient, removeClient :: (MonadFree ModelOps m) => Client -> m ()
insertClient c = liftF (InsertClient c ())
removeClient c = liftF (RemoveClient c ())


insert, remove :: (MonadFree ModelOps m) => WindowId -> m ()
insert window = liftF (InsertWindow window ())
remove window = liftF (RemoveWindow window ())


raise, lower :: (MonadFree ModelOps m) => WindowId -> m ()
raise window = liftF (Raise window ())
lower window = liftF (Lower window ())


setX, setY :: (MonadFree ModelOps m) => WindowId -> Int -> m ()
setX w v = liftF (SetX w v ())
setY w v = liftF (SetY w v ())


setWidth, setHeight :: (MonadFree ModelOps m) => WindowId -> Word -> m ()
setWidth  w v = liftF (SetWidth  w v ())
setHeight w v = liftF (SetHeight w v ())


setPosition :: (MonadFree ModelOps m) => WindowId -> Position -> m ()
setPosition  w p = liftF (SetPosition  w p ())


setDimension :: (MonadFree ModelOps m) => WindowId -> Dimension -> m ()
setDimension w d = liftF (SetDimension w d ())


setGeometry :: (MonadFree ModelOps m) => WindowId -> Geometry -> m ()
setGeometry w g = liftF (SetGeometry w g ())


setBorderColor, setBorderWidth :: (MonadFree ModelOps m) => WindowId -> Word -> m ()
setBorderColor w v = liftF (SetBorderColor w v ())
setBorderWidth w v = liftF (SetBorderWidth w v ())


member :: (Functor m, MonadFree ModelOps m) => WindowId -> m Bool
member w = Q.member w <$> getQueue


lookup :: (Functor m, MonadFree ModelOps m) => WindowId -> m (Maybe Client)
lookup w = Q.lookup w <$> getQueue


withQueueM :: (MonadFree ModelOps m) => (Queue -> m a) -> m a
withQueueM f = getQueue >>= f


toList :: (Functor m, MonadFree ModelOps m) => m [Client]
toList = Q.toList <$> getQueue


above :: Functor f => LensLike' f ClientStack [Client]
above = lens _above (\d v -> d { _above = v })

focus :: Functor f => LensLike' f ClientStack Client
focus = lens _focus (\d v -> d { _focus = v })

below :: Functor f => LensLike' f ClientStack [Client]
below = lens _below (\d v -> d { _below = v })

focusNext :: ClientStack -> ClientStack
focusNext (ClientStack [] f []) = ClientStack []          f         []
focusNext (ClientStack as f []) = ClientStack []          (head as) (tail as ++ [f])
focusNext (ClientStack as f bs) = ClientStack (as ++ [f]) (head bs) (tail bs)

focusPrev :: ClientStack -> ClientStack
focusPrev (ClientStack [] f []) = ClientStack []            f         []
focusPrev (ClientStack [] f bs) = ClientStack (f : init bs) (last bs) []
focusPrev (ClientStack as f bs) = ClientStack (as ++ [f])   (head bs) (tail bs)


integrate :: ClientStack -> [Client]
integrate (ClientStack as fc bs) = as ++ fc : bs


withClient :: (Client -> a) -> WindowId -> ClientStack -> Maybe a
withClient f w = exec . integrate
    where exec []                     = Nothing
          exec (c:cs) | c ^. xid == w = Just $ f c
                      | otherwise     = exec cs


modifyClient :: (Client -> Client) -> WindowId -> ClientStack -> ClientStack
modifyClient f w (ClientStack as fc bs)
    | w == fc ^. xid = ClientStack as (f fc) bs
    | otherwise      = ClientStack (mapf as) fc (mapf bs)
    where mapf []                     = []
          mapf (c:cs) | c ^. xid == w = f c : cs
                      | otherwise     = c : mapf cs

$(makeFree ''ClientConfig)


clientConfigs :: (Monad m)
            => ModelOps (StateT ClientConfigs (StateT Model m) a)
            -> StateT ClientConfigs (StateT Model m) ()
clientConfigs = \case
    SetX w v _ -> modcc w $ ConfigClientX w v ()
    SetY w v _ -> modcc w $ ConfigClientY w v ()
    SetWidth w v _ -> modcc w $ ConfigClientWidth w v ()
    SetHeight w v _ -> modcc w $ ConfigClientHeight w v ()
    _ -> return ()
    where modcc w c = modify $ M.alter (Just . maybe (S.singleton c) (S.insert c)) w


clientConfigs' :: (MonadState ClientConfigs m) => ModelOps t -> m ()
clientConfigs' = \case
    SetX w v _           -> modcc w $ ConfigClientX w v ()
    SetY w v _           -> modcc w $ ConfigClientY w v ()
    SetWidth w v _       -> modcc w $ ConfigClientWidth w v ()
    SetHeight w v _      -> modcc w $ ConfigClientHeight w v ()
    GrabKey w mm ks _    -> modcc w $ ConfigGrabKey w mm ks ()
    GrabButton w mm bi _ -> modcc w $ ConfigGrabButton w mm bi ()
    _                    -> return ()
    where modcc w c = modify $ M.alter (Just . maybe (S.singleton c) (S.insert c)) w


runModelOps :: Monad m => ModelOpsFT m a -> StateT ClientConfigs (StateT Model m) a
runModelOps ops = lift (lift $ runFreeT ops) >>= runModelOp
    where
    runModelOp = \case
        (Pure a)  -> return a
        (Free op) -> do
            lift $ updateModel op
            clientConfigs' op
            continue runModelOps op

    continue cont = \case
        GetQueue f   -> lift (getL queue) >>= cont . f
        PutQueue q f -> lift (putL queue q) >> cont f
        InsertClient _ f     -> cont f
        RemoveClient _ f     -> cont f
        InsertWindow _ f     -> cont f
        RemoveWindow _ f     -> cont f
        GrabKey _ _ _ f      -> cont f
        GrabButton _ _ _ f   -> cont f
        Raise _ f            -> cont f
        Lower _ f            -> cont f
        SetX _ _ f           -> cont f
        SetY _ _ f           -> cont f
        SetWidth  _ _ f      -> cont f
        SetHeight _ _ f      -> cont f
        SetPosition  _ _ f   -> cont f
        SetDimension _ _ f   -> cont f
        SetGeometry  _ _ f   -> cont f
        SetBorderColor _ _ f -> cont f
        SetBorderWidth _ _ f -> cont f
