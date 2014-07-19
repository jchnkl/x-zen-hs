-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstraintKinds, DeriveFunctor, FlexibleContexts, LambdaCase, TupleSections, TemplateHaskell #-}

module Model where


import Data.Word
import Data.Map as M
import Data.Set as S
import Data.List as L
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
-- import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Trans.Free

import Types
import Lens hiding (views)
import qualified Queue as Q
-- import qualified Window as W
import Graphics.XHB as X

{-
 TODO
 * Follow focus:
     - border color
     - warp mouse pointer when focus change by key
-}


type UpdateHints = Map WindowId (Set UpdateHint)


-- runModel :: (MonadState Model (t m), MonadState UpdateHints m, MonadTrans t)
--          => FreeT ModelOps m () -> t m ()
-- runModel = (iterTM (\m -> updateModel m >> updateHints m))
-- runModel :: Monad m => Model -> ModelOpsFT m a -> m Model

-- runModel :: Monad m => Model -> ModelOpsFT m a -> m (a, Model)
-- runModel model f = runStateT (iterTM updateModel f) model

-- runModel :: Monad m => ModelOpsFT m a -> ModelST m a
-- runModel f = (iterTM updateModel f)

-- execModel :: Monad m => Model -> ModelOpsFT m a -> m Model
-- execModel model f = execStateT (iterTM updateModel f) model

-- execModel :: Monad m => ModelOpsFT m a -> ModelST m a
-- execModel f = (iterTM updateModel f)



-- viewInterpreter :: MonadReader Model m => ModelOps (m a) -> m a
-- viewInterpreter = \case
--     GetQueue f           -> askL queue >>= f
--     PutQueue q f         -> f
--     InsertClient c f     -> f
--     RemoveClient c f     -> f
--     InsertWindow w f     -> f
--     RemoveWindow w f     -> f
--     Raise _ f            -> f -- TODO
--     Lower _ f            -> f -- TODO
--     SetX w v f           -> f
--     SetY w v f           -> f
--     SetWidth  w v f      -> f
--     SetHeight w v f      -> f
--     SetPosition  w p f   -> f
--     SetDimension w d f   -> f
--     SetGeometry  w g f   -> f
--     SetBorderColor _ _ f -> f -- TODO
--     SetBorderWidth _ _ f -> f -- TODO



-- updateModel :: (MonadState Model (t m), MonadTrans t) => ModelOps (t m a) -> t m a
-- updateModel :: (MonadState Model m) => ModelOps (m a) -> m a
updateModel :: (MonadState Model m) => ModelOps t -> m ()
updateModel = \case
    GetQueue _           -> return () -- getL queue >>= lift . f
    PutQueue _ _         -> return () -- putL queue q >> lift f
    InsertClient c _     -> insertc c
    RemoveClient c _     -> removec c
    InsertWindow w _     -> insertw w
    RemoveWindow w _     -> removew w
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
          insertw w = insertc (Client w nullPosition nullGeometry)


updateHints :: (MonadState UpdateHints m, MonadTrans t) => ModelOps (t m a) -> t m ()
updateHints = lift . \case
    Raise w _ -> modhs HintRaise w
    Lower w _ -> modhs HintLower w
    SetX w v _ -> modhs (HintX v) w
    SetY w v _ -> modhs (HintY v) w
    SetWidth  w v _ -> modhs (HintWidth  v) w
    SetHeight w v _ -> modhs (HintHeight v) w
    SetPosition  w p _ -> modhs (HintPosition  p) w
    SetDimension w d _ -> modhs (HintDimension d) w
    SetBorderColor w v _ -> modhs (HintBorderColor v) w
    SetBorderWidth w v _ -> modhs (HintBorderWidth v) w
    _ -> return ()
    where modhs h = modify . M.alter (Just . maybe (S.singleton h) (S.insert h))


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


-- modifyClientM :: (MonadState Model m) => WindowId -> (Client -> Client) -> m ()
-- modifyClientM w = modify . (queue %~) . Q.modifyClient w


{-
insertClient :: Monad m => Client -> Z m ()
insertClient = modify . (queue %~) . Q.insert


insert :: Monad m => WindowId -> Z m ()
insert window = insertClient (Client window nullPosition nullGeometry)


remove :: Monad m => WindowId -> Z m ()
remove = modify . (queue %~) . Q.remove


raise :: MonadIO m => WindowId -> Z m ()
raise = W.raise


lower :: MonadIO m => WindowId -> Z m ()
lower = W.lower


member :: Monad m => WindowId -> Z m Bool
member w = gets (^. queue . to (Q.member w))


lookup :: Monad m => WindowId -> Z m (Maybe Client)
lookup w = gets (^. queue . to (Q.lookup w))


asList :: Monad m => Z m [Client]
asList = gets (^. queue . to Q.toList)


modifyClientM :: (MonadState Model m) => WindowId -> (Client -> Client) -> m ()
modifyClientM w = modify . (queue %~) . Q.modifyClient w


withQueueM :: (Functor m, MonadIO m) => (Queue -> Z m a) -> Z m a
withQueueM f = getL queue >>= f
-}



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


-- -- modelOpToClientConfig :: ModelOps (m a) -> Maybe (ClientConfig (m a))
-- modelOpToClientConfig = \case
--     SetX w v f -> Just $ ConfigClientX w v f
--     SetY w v f -> Just $ ConfigClientY w v f
--     SetWidth w v f -> Just $ ConfigClientWidth w v f
--     SetHeight w v f -> Just $ ConfigClientHeight w v f
--     _ -> Nothing

-- modelOpToClientConfig' :: (MonadFree ClientConfig m) => ModelOps (m a) -> m ()
-- modelOpToClientConfig' = \case
--     SetX w v _ -> configClientX w v
--     SetY w v _ -> configClientY w v
--     SetWidth w v _ -> configClientWidth w v
--     SetHeight w v _ -> configClientHeight w v
--     _ -> return ()


-- clientConfigs :: (MonadState ClientConfigs m, MonadTrans t) => ModelOps (t m a) -> m ()
-- clientConfigs :: (MonadState ClientConfigs m) => ModelOps (m a) -> m ()
-- clientConfigs :: (MonadState ClientConfigs m) => ModelOps t -> m ()
-- (Map WindowId (Set (ClientConfig ()))) (StateT Model (SetupRT IO)) ()
-- clientConfigs :: (Monad m) => ModelOps (StateT ClientConfigs m a) -> StateT ClientConfigs m ()

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
    SetX w v _ -> modcc w $ ConfigClientX w v ()
    SetY w v _ -> modcc w $ ConfigClientY w v ()
    SetWidth w v _ -> modcc w $ ConfigClientWidth w v ()
    SetHeight w v _ -> modcc w $ ConfigClientHeight w v ()
    _ -> return ()
    where modcc w c = modify $ M.alter (Just . maybe (S.singleton c) (S.insert c)) w

-- runModel' f = execStateT (iterT modelUpdate' f) M.empty

-- runViews' views = forM_ views . \f -> uncurry (runReaderT . runReaderT f)
-- runViews' :: (Monad m, MonadReader Model (t m), MonadReader ClientConfigs m, MonadTrans t)
-- runViews' :: (Monad m) => [model -> clientconfigs -> m ()] -> (model, clientconfigs) -> m ()
-- runViews' views = forM_ views . flip uncurry

runViews :: ClientConfigs -> ModelST (SetupRT IO) ()
runViews c = do
    viewfs <- asksL (config . views) $ L.map ($ c)
    get >>= lift . runReaderT (sequence_ viewfs)

    -- where
    -- run :: Model -> ClientConfigs -> [View] -> SetupRT IO ()
    -- run m c = mapM_ (\f -> f m c)

-- > let r1 = (runStateT . execStateT (iterT modelUpdate' testModelOp)) testModel M.empty
-- > let r2 = uncurry (runReaderT . runReaderT (printView''' ""))
-- > r1 >>= r2

-- io :: (MonadIO m) => IO a -> m a
-- io = liftIO

-- startupComponent :: MonadIO m => FreeT ModelOps (SetupRT m) a -> StateT Model (SetupRT m) ClientConfigs
-- startupComponent f = do
--     io $ putStr "startupComponent (model before): "
--     get >>= io . putStrLn . show
--     -- res <- execStateT (runModelOps f) M.empty
--     res <- undefined
--     io $ putStr "startupComponent (model after): "
--     get >>= io . putStrLn . show
--     return res

    -- where io = liftIO

    -- execStateT (modelUpdate' f) undefined
    -- let ops = undefined
    -- let views = undefined
    -- configs <- execStateT (modelUpdate' ops) M.empty
    -- get >>= flip (runViews' views) configs
    -- ccs <- execStateT (modelUpdate' undefined) M.empty
    -- let op = undefined
    -- updateModel op
    -- execStateT (clientConfigs op) M.empty
    -- return M.empty
    -- execStateT ccs M.empty

-- modelUpdate' :: (MonadState Model (t m), MonadState ClientConfigs m, MonadTrans t)
--             => ModelOps (t m a) -> t m a
-- modelUpdate' :: (MonadState Model m, MonadTrans t)
--              => ModelOps (t m a)
--              -> t m a
-- modelUpdate' :: (MonadTrans t, MonadState Model (t m), MonadState ClientConfigs m)
--              => ModelOps (t m b) -> t m b
-- modelUpdate' :: (MonadTrans t, MonadState Model m, MonadState ClientConfigs (t m))
--              => ModelOps (t m b) -> t m b
-- modelUpdate' op = do
--     (clientConfigs op)
--     (updateModel op)
-- modelUpdate' op = likgclientConfigs op >> lift (updateModel op)

-- modelUpdate' :: (Monad m)
             -- => FreeT ModelOps (StateT ClientConfigs (StateT Model m)) a
             -- -> StateT ClientConfigs (StateT Model m) a
-- modelUpdate' = iterT (\op -> {- (clientConfigs op) >> -} (updateModel op))


-- type X m = LogWT ( FreeT ModelOps (SetupRT m))

-- execModelOps :: (Monad m, MonadTrans t, Functor (t m), MonadState Model (t m))
--              => ModelOpsFT m a -> t m ClientConfigs
-- execModelOps = fmap snd . runModelOps'

-- runModelOps'' :: (Monad m, MonadTrans t, MonadState Model (t m))
--             => FreeT ModelOps m a -> ClientConfigs -> t m (a, ClientConfigs)
-- runModelOps'' f cs = runStateT (runModelOps f) cs

-- runModelOps' :: (Monad m, MonadTrans t, MonadState Model (t m))
--             => FreeT ModelOps m a -> t m (a, ClientConfigs)
-- runModelOps' = flip runStateT M.empty . runModelOps

-- runModelOps''' :: (Monad m)
--             => ModelOpsFT m a -> StateT Model (StateT ClientConfigs m) a
-- runModelOps''' = runModelOps

-- runModelOps :: (Monad m, MonadTrans t, MonadTrans t1,
--                 MonadState ClientConfigs (t1 m), MonadState Model (t (t1 m)))
--             => FreeT ModelOps m a -> t (t1 m) a
-- runModelOps :: (Monad m)
--             => FreeT ModelOps m a -> StateT Model (StateT ClientConfigs m) a

-- runModelOps :: 
-- ||                  (MonadState Model (t (t1 (t2 m))), MonadState Model m,
-- ||                   MonadState ClientConfigs (t1 (t2 m)), MonadTrans t2, MonadTrans t1,
-- ||                   MonadTrans t, Monad (t2 m)) =>
-- ||                  FreeT ModelOps (t2 m) a -> t (t1 (t2 m)) a

runModelOps' :: Monad m => ModelOpsFT m a -> StateT ClientConfigs (StateT Model m) a
runModelOps' ops = lift (lift $ runFreeT ops) >>= runModelOp
    where
    runModelOp = \case
        (Pure a)  -> return a
        (Free op) -> do
            lift $ updateModel op
            clientConfigs' op
            continue runModelOps' op

    continue cont = \case
        GetQueue f   -> lift (getL queue) >>= cont . f
        PutQueue q f -> lift (putL queue q) >> cont f
        InsertClient _ f     -> cont f
        RemoveClient _ f     -> cont f
        InsertWindow _ f     -> cont f
        RemoveWindow _ f     -> cont f
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

runModelOps :: Monad m => ModelOpsFT m a -> StateT Model (StateT ClientConfigs m) a
runModelOps ops = lift (lift $ runFreeT ops) >>= runModelOp
    where
    runModelOp = \case
        (Pure a)  -> return a
        (Free op) -> do
            updateModel op
            lift $ clientConfigs' op
            continue runModelOps op

    continue cont = \case
        GetQueue f   -> getL queue >>= cont . f
        PutQueue q f -> putL queue q >> cont f
        InsertClient _ f     -> cont f
        RemoveClient _ f     -> cont f
        InsertWindow _ f     -> cont f
        RemoveWindow _ f     -> cont f
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



-- -- printView''' :: (MonadReader Model m, MonadReader (Map WindowId (Set (ClientConfig ()))) (t m), MonadIO (t m), MonadTrans t)
-- modelUpdate :: (MonadState Model (t m), MonadFree ClientConfig (t m), MonadTrans t)
--             => ModelOps (t m a) -> t m a
-- modelUpdate op = do
--     -- case op of
--     --     SetX w v _ -> configClientX w v >> return ()
--     modelOpToClientConfig' op
--     updateModel op


-- -- runView :: (Monad m) => ClientConfig (m Model) -> m Model
-- -- printView'' :: 
-- -- runView :: ((MonadReader Model m, MonadIO m) => ClientConfig (m a) -> m a) -> m Model
-- runView f' model = \case
--     ConfigClientX _ _ f -> run f
--     ConfigClientY _ _ f -> run f
--     ConfigClientWidth _ _ f -> run f
--     ConfigClientHeight _ _ f -> run f
--     where
--     run f = do
--         model <- f
--         runReaderT f' model
--         return model

-- iterT (printView "") (execStateT (iterTM modelUpdate (setX (fromXid xidNone::WindowId) 0 )) (undefined :: Model))
--   :: (MonadReader Model m1, MonadIO m1, MonadTrans t, Monad (t m),
--         Monad m) =>
--              ((ClientConfig (m1 a) -> m1 a)
--                    -> ClientConfig (t m Model) -> t m Model)
--                         -> t m Model

printView'''' :: MonadIO m => Model -> ClientConfigs -> m ()
printView'''' model ccs = liftIO $ do
    putStrLn . ("printView''' Model: " ++) . show $ model
    putStrLn . ("printView''' ClientConfig: " ++) . show $ ccs


printView''' :: (MonadReader Model (t m), MonadReader (Map WindowId (Set (ClientConfig ()))) m, MonadIO (t m), MonadTrans t)
             => String -> t m ()
printView''' str = do
    liftIO . putStrLn $ str -- ++ ": " ++ show cc
    ask >>= liftIO . print . ("printView''' Model: " ++) . show
    lift ask >>= liftIO . print . ("printView''' ClientConfig: " ++) . show


printView'' :: (MonadReader Model m, MonadIO m) => String -> ClientConfig (m a) -> m a
printView'' str cc = do
    liftIO . putStrLn $ str ++ ": " ++ show cc
    case cc of
        ConfigClientX _ _ f -> do
            model <- ask
            liftIO . print $ "printView'' Model: " ++ show model
            -- return model
            f
        ConfigClientY _ _ f -> do
            f
        ConfigClientWidth _ _ f -> do
            f
        ConfigClientHeight _ _ f -> do
            f


printView' :: (MonadIO m) => String -> ClientConfig (ReaderT Model m a) -> ReaderT Model m a
printView' str cc = do
    ask >>= liftIO . putStrLn . ("Model: " ++) . show
    liftIO . putStrLn $ str ++ ": " ++ show cc
    case cc of
        ConfigClientX _ _ f -> f
        ConfigClientY _ _ f -> f
        ConfigClientWidth _ _ f -> f
        ConfigClientHeight _ _ f -> f


printView :: (MonadIO m) => String -> ClientConfig (m a) -> m a
printView str cc = do
    liftIO . putStrLn $ str ++ ": " ++ show cc
    case cc of
        ConfigClientX _ _ f -> f
        ConfigClientY _ _ f -> f
        ConfigClientWidth _ _ f -> f
        ConfigClientHeight _ _ f -> f


-- foo model f = do
--     model' <- execStateT (iterTM modelUpdate f) model
--     printView model'

testModelOp :: (MonadFree ModelOps m) => m ()
testModelOp = do
    setX (fromXid xidNone :: WindowId) 1
    setY (fromXid xidNone :: WindowId) 2

testModel :: Model
testModel = Model
           $ ClientQueue []
                         (Just $ Client (fromXid xidNone :: WindowId)
                                        nullPosition
                                        nullGeometry)
                         []
