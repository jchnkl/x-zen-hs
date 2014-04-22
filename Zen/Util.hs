{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Util where

import qualified Data.Map as M
import Control.Monad.Writer
import Control.Applicative
import Graphics.XHB
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


subLists :: Int -> [a] -> [[a]]
subLists _ [] = []
subLists n lst = take n lst : subLists n (drop n lst)


-- http://tronche.com/gui/x/xlib/input/XGetKeyboardMapping.html
-- http://cgit.freedesktop.org/~arnau/xcb-util/tree/keysyms/keysyms.c
-- -> xcb_key_symbols_get_keysym

keyboardMapping :: Connection -> Receipt GetKeyboardMappingReply
                -> IO (M.Map KEYCODE [KEYSYM])
keyboardMapping c receipt = keycodes' <$> getReply receipt
    where
    keycodes' (Left _) = M.empty
    keycodes' (Right reply) =
        let min_keycode = min_keycode_Setup $ connectionSetup c
            ks_per_kc = fi $ keysyms_per_keycode_GetKeyboardMappingReply reply
            keysyms = subLists ks_per_kc $ keysyms_GetKeyboardMappingReply reply
        in M.fromList $ zip [min_keycode ..] keysyms


keysymToKeycode :: KEYSYM -> M.Map KEYCODE [KEYSYM] -> Maybe KEYCODE
keysymToKeycode keysym = safeHead . M.keys . M.filter (fi keysym `elem`)



-- modifierMapping :: Connection -> Receipt GetModifierMappingReply ->
--                 -> IO (M.Map KEYCODE [Mod
