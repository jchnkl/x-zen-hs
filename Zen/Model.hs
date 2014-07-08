-- vim:sw=4:sts=4:ts=4

-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFunctor, FlexibleContexts, LambdaCase, TupleSections #-}

module Model where


import Data.Word
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Data.Map as M
import Data.Set as S
import Data.List as L
-- import qualified Data.Map as M
-- import Data.List ((\\))
-- import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Control.Monad.Free as F
import Control.Monad.Trans.Free
-- import Control.Applicative
-- import Graphics.XHB
import Types
import Util
import Lens
-- import Log
-- import Message
import qualified Queue as Q
import qualified Window as W
-- import qualified Keyboard as K
-- import Graphics.X11.Types (KeySym, xK_Num_Lock, xK_Caps_Lock)
import Graphics.XHB as X hiding (Setup, Property)

{-
 TODO
 * Follow focus:
     - border color
     - warp mouse pointer when focus change by key
-}


type UpdateHints = Map WindowId (Set UpdateHint)


updateModel :: (MonadState Model (t m), MonadTrans t) => ModelOps (t m a) -> t m a
updateModel = \case
    GetModel f           -> get >>= f
    Raise _ f            -> f -- TODO
    Lower _ f            -> f -- TODO
    SetX w v f           -> modc w (geometry . position . x .~ v) >> f
    SetY w v f           -> modc w (geometry . position . y .~ v) >> f
    SetWidth  w v f      -> modc w (geometry . dimension . width  .~ v) >> f
    SetHeight w v f      -> modc w (geometry . dimension . height .~ v) >> f
    SetPosition  w p f   -> modc w (geometry . position .~ p) >> f
    SetDimension w d f   -> modc w (geometry . dimension .~ d) >> f
    SetBorderColor _ _ f -> f -- TODO
    SetBorderWidth _ _ f -> f -- TODO
    where modc w f = modify $ model %~ Q.modifyClient w f


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


xcbView :: (MonadIO m, MonadReader Setup m) => Model -> UpdateHints -> m ()
xcbView _ = mapM_ (uncurry configure) . M.toList
          . M.map (toValueParam . concat . L.map hintToValueParam . S.toList)
    where
    configure w vp = connection $-> \c -> io $ X.configureWindow c w vp
    hintToValueParam = \case
        HintX v -> [(ConfigWindowX, fi v)]
        HintY v -> [(ConfigWindowY, fi v)]
        HintWidth v -> [(ConfigWindowWidth, fi v)]
        HintHeight v -> [(ConfigWindowHeight, fi v)]
        HintPosition p -> [(ConfigWindowX, fi $ p ^. x), (ConfigWindowY, fi $ p ^. y)]
        HintDimension d -> [(ConfigWindowWidth, fi $ d ^. width), (ConfigWindowHeight, fi $ d ^. height)]
        _ -> []
        -- HintRaise w
        -- HintLower w
        -- HintBorderColor v) w
        -- HintBorderWidth v) w

-- runModelWith :: MonadState Model m => (a -> m b) -> ModelOps a -> m b
-- runModelWith t = \case
--     GetModel f           -> get >>= t . f
--     Raise w f            -> t f -- TODO
--     Lower w f            -> t f -- TODO
--     SetX w v f           -> modc w (geometry . position . x .~ v) >> t f
--     SetY w v f           -> modc w (geometry . position . y .~ v) >> t f
--     SetWidth  w v f      -> modc w (geometry . dimension . width  .~ v) >> t f
--     SetHeight w v f      -> modc w (geometry . dimension . height .~ v) >> t f
--     SetPosition  w p f   -> t f -- TODO
--     SetDimension w d f   -> t f -- TODO
--     SetBorderColor w v f -> t f -- TODO
--     SetBorderWidth w v f -> t f -- TODO
--     where modc w f = modify $ model %~ Q.modifyClient w f

-- setWidth, setHeight :: WindowId -> Int -> FreeT ModelOpsF ()
-- setWidth  = geometry . position . width  .~
-- setHeight = geometry . position . height .~


-- data ClientProperties = ClientProperties
--     { _client     :: WindowId
--     , _properties :: [Property]
--     }
--     deriving (Show, Typeable)

-- data UpdateHints a = UpdateHints [(WindowId, [ClientProperty])] a
--     deriving (Functor, Show, Typeable)

-- type UpdateHintsFT = FreeT UpdateHints


-- runUpdateHints :: UpdateHints (IO a) -> IO a
-- runUpdateHints (UpdateHints hints f) = print hints >> f


-- runFreeModel :: ModelOps (StateT Model IO ()) -> FreeT ModelOps (StateT Model IO) ()
-- runFreeModel = \case
--     (Move   w p f) -> do
--         move w p
--         modify ((model %~) (Q.modifyClient w $ geometry . position  .~ p))
--         lift f
--     (Resize w d f) -> do
--         resize w d
--         modify ((model %~) (Q.modifyClient w $ geometry . dimension .~ d))
--         lift f


-- -- move :: Monad m => WindowId -> Position -> ModelFT m ()
-- move :: MonadFree ModelOps m => WindowId -> Position -> m ()
-- move w p = liftF (Move w p ())


-- resize :: MonadFree ModelOps m => WindowId -> Dimension -> m ()
-- resize w d = liftF (Resize w d ())


-- runModelOpsF :: Connection -> ModelOps (State Model a) -> State Model a
-- runModelOpsF c = \case
--     (Move   w p f) -> return () >> f
--     (Resize w d f) -> return () >> f


-- type ModelRT = ReaderT Model
-- model <- ask
-- but modify:
-- M.move window position
-- M.resize window dimension
-- etc.


-- run one component:
-- update model
-- after all components:
-- run view with (model, modelops)


-- type Z m = LogWT (StateT Model (SetupRT m))

type ComponentStack m = LogWT (ReaderT Model (FreeT ModelOps (SetupRT m)))

type Y' m = LogWT (ReaderT Model (SetupRT m))

-- bar   f' = runReaderT
--             (iterT f'
--                 (runReaderT
--                     (runWriterT undefined)
--                 (undefined :: Model))
--             )
--         (undefined :: Setup)

-- fro = bar updateModel

-- foo :: Z IO ()
-- foo = do
--     _model <- get
--     put $ updateModel undefined

-- runModel :: 

-- -- updateModel :: (MonadState Model m) => ModelOps (m a) -> m a
-- updateModel :: ModelOps Model -> Model
-- updateModel m = case m of
--     (Move   w p cm) -> (model %~) (Q.modifyClient w $ geometry . position  .~ p) cm
--     (Resize w d cm) -> (model %~) (Q.modifyClient w $ geometry . dimension .~ d) cm
--         -- (Resize w d cm) -> modifyClientM w (geometry . dimension .~ d) >> f


-- -- runModelOpsX :: MonadIO m => ModelOps (m a) -> m a
-- runModelOpsX :: (MonadReader Setup m, MonadIO m) => ModelOps (m a) -> m a
-- runModelOpsX m = do
--     c <- askL connection
--     case m of
--         (Move w p f) -> do
--             io $ X.configureWindow c w $ toValueParam [(ConfigWindowX, fi $ p ^. x),
--                                                        (ConfigWindowY, fi $ p ^. y)]
--             f
--
--         (Resize w d f) -> do
--             io $ X.configureWindow c w $ toValueParam [(ConfigWindowWidth,  fi $ d ^. width),
--                                                        (ConfigWindowHeight, fi $ d ^. height)]
--             f
--
--
--
insertClient :: Monad m => Client -> Z m ()
insertClient = modify . (model %~) . Q.insert


insert :: Monad m => WindowId -> Z m ()
insert window = insertClient (Client window nullPosition nullGeometry)


remove :: Monad m => WindowId -> Z m ()
remove = modify . (model %~) . Q.remove


raise :: MonadIO m => WindowId -> Z m ()
raise = W.raise


lower :: MonadIO m => WindowId -> Z m ()
lower = W.lower


member :: Monad m => WindowId -> Z m Bool
-- member w = gets (^. (model . to (Q.member w)))
member w = gets (^. model . to (Q.member w))
-- \w -> gets ( ^. (model . to (member w)))
-- :t m ^. model . (to $ member (undefined :: WindowId ))


lookup :: Monad m => WindowId -> Z m (Maybe Client)
lookup w = gets (^. model . to (Q.lookup w))


asList :: Monad m => Z m [Client]
asList = gets (^. model . to Q.toList)


modifyClientM :: (MonadState Model m) => WindowId -> (Client -> Client) -> m ()
modifyClientM w = modify . (model %~) . Q.modifyClient w


withQueueM :: (Functor m, MonadIO m) => (Queue -> Z m a) -> Z m a
withQueueM f = getL model >>= f



-- runModel :: Monad m => ModelT m a -> ClientStack -> m (a, ClientStack)
-- runModel = runStateT

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


-- data Model = Model { _clients :: ClientStack }
--     deriving Typeable
--
-- clients :: Functor f => LensLike' f Model ClientStack
-- clients = lens _clients (\d v -> d { _clients = v })

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
