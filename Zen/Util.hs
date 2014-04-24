{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Util where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Writer
import Graphics.XHB

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

keysymToKeycode :: KEYSYM -> Map KEYCODE [KEYSYM] -> Maybe KEYCODE
keysymToKeycode keysym = safeHead . M.keys . M.filter (keysym `elem`)

keycodeToKeysym :: KEYCODE -> Map KEYCODE [KEYSYM] -> [KEYSYM]
keycodeToKeysym keycode = fromMaybe [] . M.lookup keycode
