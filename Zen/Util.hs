{-# OPTIONS_GHC -Wall #-}

module Util where

import Control.Monad.Writer
import Graphics.XHB
import Types

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

convertXid :: (XidLike a, XidLike b) => a -> b
convertXid = fromXid . toXid

unlessZ :: Z Bool -> Z () -> Z ()
unlessZ zb f = do
    b <- zb
    unless b f

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

toLog :: String -> Z ()
toLog s = tell [s]
