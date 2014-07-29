-- vim:sw=4:sts=4:ts=4

{-# LANGUAGE LambdaCase #-}

module Util where

import Control.Monad.Writer
import Control.Concurrent.STM
import Graphics.XHB
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor
import Types

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral


io :: MonadIO m => IO a -> m a
io = liftIO


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a


safeTail :: [a] -> [a]
safeTail [] = []
safeTail ls = tail ls


rotate :: Int -> [t] -> [t]
rotate n ls = [ l | l <- drop n ls] ++ [ r | r <- take n ls]


convertXid :: (XidLike a, XidLike b) => a -> b
convertXid = fromXid . toXid


whenId :: Monoid a => Bool -> a -> a
whenId b a = if b then a else mempty


whenM :: Monad m => m Bool -> m () -> m ()
whenM mb f = mb >>= flip when f


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb f = mb >>= flip unless f


whenJust :: Maybe a -> (a -> b) -> (Maybe b)
whenJust v f = fmap f v


whenJustM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenJustM Nothing _ = return Nothing
whenJustM (Just v) f = liftM Just (f v)


whenJustM_ :: (Functor m, Monad m) => Maybe a -> (a -> m b) -> m ()
whenJustM_ v = void . whenJustM v

fromRight :: b -> Either a b -> b
fromRight _ (Right v) = v
fromRight v _         = v


whenRight :: Either a b -> (b -> c) -> Maybe c
whenRight (Left _)  _ = Nothing
whenRight (Right v) f = Just (f v)


whenRightM :: Monad m => Either a b -> (b -> m c) -> m (Maybe c)
whenRightM (Left _)  _ = return Nothing
whenRightM (Right v) f = liftM Just (f v)


whenRightM_ :: (Functor m, Monad m) => Either a b -> (b -> m c) -> m ()
whenRightM_ v = void . whenRightM v


withTMVar :: MonadIO m => TMVar a -> (a -> b) -> m b
withTMVar var f = withTMVarM var (return . f)


withTMVarM :: MonadIO m => TMVar a -> (a -> m b) -> m b
withTMVarM var f = liftIO (atomically $ readTMVar var) >>= f


modifyTMVar :: MonadIO m => TMVar a -> (a -> a) -> m ()
modifyTMVar var f = modifyTMVarM var (return . f)


modifyTMVarM :: MonadIO m => TMVar a -> (a -> m a) -> m ()
modifyTMVarM var f = get >>= f >>= put
    where get = liftIO . atomically $ takeTMVar var
          put = liftIO . atomically . putTMVar var


reply :: (MonadIO m, Functor m) => Receipt a -> m (Either SomeError a)
reply = io . getReply


getReplies :: [Receipt a] -> IO (Either SomeError [a])
getReplies = fmap replies . mapM getReply
    where
    replies :: [Either SomeError a] -> Either SomeError [a]
    replies (Right r : receipts) = replies receipts >>= Right . (r :)
    replies (Left e : _) = Left e
    replies _ = Right []


partition :: Int -> [a] -> [[a]]
partition _ [] = []
partition n lst = take n lst : partition n (drop n lst)


-- [1,2,3] -> [[1],[1,2],[1,3],[2],[2,3],[3]]
combinations :: [a] -> [[a]]
combinations    []  = []
combinations (u:[]) = [[u]]
combinations (u:us) = [u] : map (\v -> [u,v]) us ++ combinations us


getEdges :: Geometry -> (Maybe Edge, Maybe Edge)
getEdges (Geometry (Position x' y') (Dimension w' h'))
    -- 360 / 8 = 45; 45 / 2 = 22.5
    | angle >  22.5 && angle <=  67.5 = (Just North, Just East)
    | angle >  67.5 && angle <= 112.5 = (Nothing,    Just East)
    | angle > 112.5 && angle <= 157.5 = (Just South, Just East)
    | angle > 157.5 && angle <= 202.5 = (Just South, Nothing)
    | angle > 202.5 && angle <= 247.5 = (Just South, Just West)
    | angle > 247.5 && angle <= 292.5 = (Nothing,    Just West)
    | angle > 292.5 && angle <= 337.5 = (Just North, Just West)
    | otherwise                       = (Just North, Nothing)
    where
    norm_x = (fi x' - fi w' / 2.0) / (fi w' / 2.0)
    norm_y = (fi y' - fi h' / 2.0) / (fi h' / 2.0)
    angle = (180.0 / pi :: Double) * ((pi :: Double) - atan2 (norm_x) (norm_y))


getCursor :: (Maybe Edge, Maybe Edge) -> Glyph
getCursor = \case
    (Just North, Nothing)   -> xC_top_side
    (Just North, Just East) -> xC_top_right_corner
    (Just North, Just West) -> xC_top_left_corner
    (Just South, Nothing)   -> xC_bottom_side
    (Just South, Just East) -> xC_bottom_right_corner
    (Just South, Just West) -> xC_bottom_left_corner
    (Nothing,    Just East) -> xC_right_side
    (Nothing,    Just West) -> xC_left_side
    _                       -> xC_sizing
