{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Util where

import Control.Monad.Writer
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

convertXid :: (XidLike a, XidLike b) => a -> b
convertXid = fromXid . toXid

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb f = mb >>= flip when f

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb f = mb >>= flip unless f

whenJust :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenJust Nothing _ = return Nothing
whenJust (Just v) f = liftM Just (f v)

whenRight :: Monad m => Either a b -> (b -> m c) -> m (Maybe c)
whenRight (Left _)  _ = return Nothing
whenRight (Right v) f = liftM Just (f v)

getReplies :: [Receipt a] -> IO (Either SomeError [a])
getReplies = fmap replies . mapM getReply
    where
    replies :: [Either SomeError a] -> Either SomeError [a]
    replies (Right reply : receipts) = replies receipts >>= Right . (reply :)
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
