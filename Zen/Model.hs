-- vim:sw=4:sts=4:ts=4

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

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
import Control.Monad.Trans.Free
import Graphics.X11 (KeySym)
import Graphics.XHB (ModMask, ButtonIndex)

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


grabKey :: (MonadFree ModelOps m) => WindowId -> KeySym -> [ModMask] -> m ()
grabKey w ks mm = liftF (GrabKey w ks mm ())


grabButton :: (MonadFree ModelOps m) => WindowId -> ButtonIndex -> [ModMask] -> m ()
grabButton w bi mm = liftF (GrabButton w bi mm ())


member :: (Functor m, MonadFree ModelOps m) => WindowId -> m Bool
member w = Q.member w <$> getQueue


lookup :: (Functor m, MonadFree ModelOps m) => WindowId -> m (Maybe Client)
lookup w = Q.lookup w <$> getQueue


withQueueM :: (MonadFree ModelOps m) => (Queue -> m a) -> m a
withQueueM f = getQueue >>= f


toList :: (Functor m, MonadFree ModelOps m) => m [Client]
toList = Q.toList <$> getQueue


updateModel :: (MonadState Model m) => ModelOps t -> m ()
updateModel = \case
    InsertClient c _     -> insertc c
    RemoveClient c _     -> removec c
    InsertWindow w _     -> insertw w
    RemoveWindow w _     -> removew w
    SetX w v _           -> modc w (geometry . position . x .~ v)
    SetY w v _           -> modc w (geometry . position . y .~ v)
    SetWidth  w v _      -> modc w (geometry . dimension . width  .~ v)
    SetHeight w v _      -> modc w (geometry . dimension . height .~ v)
    SetPosition  w p _   -> modc w (geometry . position .~ p)
    SetDimension w d _   -> modc w (geometry . dimension .~ d)
    SetGeometry  w g _   -> modc w (geometry .~ g)
    _                    -> return ()

    where modc w f = modify $ queue %~ Q.modifyClient w f
          removec = removew . (^. xid)
          insertc = modify . (queue %~) . Q.insert
          removew = modify . (queue %~) . Q.remove
          insertw w = insertc (Client w nullPosition nullGeometry)


clientConfigs :: (MonadState ClientConfigs m) => ModelOps t -> m ()
clientConfigs = \case
    Raise w _              -> modcc w $ ConfigClientRaise
    Lower w _              -> modcc w $ ConfigClientLower
    SetX w v _             -> modcc w $ ConfigClientX v
    SetY w v _             -> modcc w $ ConfigClientY v
    SetWidth w v _         -> modcc w $ ConfigClientWidth v
    SetHeight w v _        -> modcc w $ ConfigClientHeight v
    SetBorderColor w v _   -> modcc w $ ConfigClientBorderColor v
    SetBorderWidth w v _   -> modcc w $ ConfigClientBorderWidth v
    GrabKey w mm ks _      -> modcc w $ ConfigGrabKey mm ks
    UngrabKey w mm ks _    -> modcc w $ ConfigUngrabKey mm ks
    GrabButton w mm bi _   -> modcc w $ ConfigGrabButton mm bi
    UngrabButton w mm bi _ -> modcc w $ ConfigUngrabButton mm bi
    _                      -> return ()
    where modcc w c = modify $ M.alter (Just . maybe (S.singleton c) (S.insert c)) w


runModelOps :: Monad m => ModelOpsFT m a -> StateT ClientConfigs (StateT Model m) a
runModelOps ops = lift (lift $ runFreeT ops) >>= runModelOp
    where
    runModelOp = \case
        Pure a  -> return a
        Free op -> do
            lift $ updateModel op
            clientConfigs op
            continue runModelOps op

    continue cont = \case
        GetQueue f   -> lift (getL queue) >>= cont . f
        PutQueue q f -> lift (putL queue q) >> cont f
        InsertClient _ f     -> cont f
        RemoveClient _ f     -> cont f
        InsertWindow _ f     -> cont f
        RemoveWindow _ f     -> cont f
        GrabKey _ _ _ f      -> cont f
        UngrabKey _ _ _ f    -> cont f
        GrabButton _ _ _ f   -> cont f
        UngrabButton _ _ _ f -> cont f
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
