{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}

module Util where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
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

keysymToKeycode :: KEYSYM -> Map KEYCODE [KEYSYM] -> Maybe KEYCODE
keysymToKeycode keysym = safeHead . M.keys . M.filter (keysym `elem`)

keycodeToKeysym :: KEYCODE -> Map KEYCODE [KEYSYM] -> [KEYSYM]
keycodeToKeysym keycode = fromMaybe [] . M.lookup keycode

keycodeToModifier :: KEYCODE -> Map MapIndex [KEYCODE] -> Maybe MapIndex
keycodeToModifier keycode = safeHead . M.keys . M.filter (keycode `elem`)

keysymToModifier :: KEYSYM -> Map KEYCODE [KEYSYM] -> Map MapIndex [KEYCODE]
                 -> Maybe MapIndex
keysymToModifier keysym kbdmap modmap =
    keysymToKeycode keysym kbdmap >>= flip keycodeToModifier modmap

modifierToKeycode :: MapIndex -> Map MapIndex [KEYCODE] -> [KEYCODE]
modifierToKeycode = M.findWithDefault []


getEdges :: Geometry -> (Edge, Edge)
getEdges (Geometry (Position x' y') (Dimension w' h'))
    -- 360 / 8 = 45; 45 / 2 = 22.5
    | angle >  22.5 && angle <=  67.5 = (North, East)
    | angle >  67.5 && angle <= 112.5 = (None,  East)
    | angle > 112.5 && angle <= 157.5 = (South, East)
    | angle > 157.5 && angle <= 202.5 = (South, None)
    | angle > 202.5 && angle <= 247.5 = (South, West)
    | angle > 247.5 && angle <= 292.5 = (None,  West)
    | angle > 292.5 && angle <= 337.5 = (North, West)
    | otherwise = (North, None)
    where
    norm_x = (fi x' - fi w' / 2.0) / (fi w' / 2.0)
    norm_y = (fi y' - fi h' / 2.0) / (fi h' / 2.0)
    angle = (180.0 / pi :: Double) * ((pi :: Double) - atan2 (norm_x) (norm_y))


getCursor :: (Edge, Edge) -> Glyph
getCursor = \case
    (North, None) -> xC_top_side
    (North, East) -> xC_top_right_corner
    (North, West) -> xC_top_left_corner
    (South, None) -> xC_bottom_side
    (South, East) -> xC_bottom_right_corner
    (South, West) -> xC_bottom_left_corner
    (None,  East) -> xC_right_side
    (None,  West) -> xC_left_side
    _             -> xC_sizing
