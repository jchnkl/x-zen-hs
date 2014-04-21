{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Util where

import Data.Maybe

import qualified Data.Map as M
import Control.Monad.Writer
import Control.Applicative
import Graphics.XHB
import Graphics.X11.Types hiding (Connection)
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


subLists :: Int -> [a] -> [[a]]
subLists _ [] = []
subLists n lst = take n lst : subLists n (drop n lst)


-- http://tronche.com/gui/x/xlib/input/XGetKeyboardMapping.html
-- http://cgit.freedesktop.org/~arnau/xcb-util/tree/keysyms/keysyms.c
-- -> xcb_key_symbols_get_keysym

getKeycode' :: Connection -> IO (M.Map KEYCODE [KEYSYM])
getKeycode' c = keycodes c =<<
    getKeyboardMapping c min_keycode (max_keycode - min_keycode + 1)
    where setup = connectionSetup c
          min_keycode = min_keycode_Setup setup
          max_keycode = max_keycode_Setup setup

getKeycode :: Connection -> KEYSYM -> IO (Maybe KEYCODE)
getKeycode c keysym = do
    let setup = connectionSetup c
        min_keycode = min_keycode_Setup setup
        max_keycode = max_keycode_Setup setup

    fmap (+ min_keycode) . getKeycodeFromReply <$> (getReply =<<
        getKeyboardMapping c min_keycode (max_keycode - min_keycode + 1))

    where
    getKeycodeFromReply :: Either SomeError GetKeyboardMappingReply -> Maybe KEYCODE
    getKeycodeFromReply (Left _) = Nothing
    getKeycodeFromReply (Right reply) = findKeycode (subLists (fi ks_per_kc) ks)
        where ks = keysyms_GetKeyboardMappingReply reply
              ks_per_kc = keysyms_per_keycode_GetKeyboardMappingReply reply

    findKeycode :: [[KEYSYM]] -> Maybe KEYCODE
    findKeycode = findKeycode' 0
        where
        findKeycode' n (keysyms:keysymslst)
            | keysym `elem` keysyms = Just n
            | otherwise             = findKeycode' (n+1) keysymslst
        findKeycode' _ _ = Nothing


    -- where
    -- getKeycodeFromReply (Left _) = Nothing
    -- getKeycodeFromReply (Right reply) = findKeycode (subLists (fi ks_per_kc) ks)
    --     where ks = keysyms_GetKeyboardMappingReply reply
    --           ks_per_kc = keysyms_per_keycode_GetKeyboardMappingReply reply

keycodes :: Connection -> Receipt GetKeyboardMappingReply -> IO (M.Map KEYCODE [KEYSYM])
keycodes c receipt = keycodes' <$> getReply receipt
-- keycodes c (Right reply) = M.fromList $ zip [min_keycode ..] keysyms
    -- fmap (+ min_keycode) . getKeycodeFromReply <$> (getReply =<<
    --     getKeyboardMapping c min_keycode (max_keycode - min_keycode + 1))

    where
    keycodes' (Left _) = M.empty
    keycodes' (Right reply) =
        let min_keycode = min_keycode_Setup $ connectionSetup c
            keysyms_per_keycode = fi $ keysyms_per_keycode_GetKeyboardMappingReply reply
            keysyms = subLists keysyms_per_keycode $ keysyms_GetKeyboardMappingReply reply
        in M.fromList $ zip [min_keycode ..] keysyms

    -- getKeycodeFromReply :: GetKeyboardMappingReply -> Maybe KEYCODE
    -- getKeycodeFromReply reply = findKeycode (subLists (fi ks_per_kc) ks)
    --     where ks = keysyms_GetKeyboardMappingReply reply
    --           ks_per_kc = keysyms_per_keycode_GetKeyboardMappingReply reply

    -- findKeycode :: [[KEYSYM]] -> Maybe KEYCODE
    -- findKeycode = findKeycode' 0
    --     where
    --     findKeycode' n (keysyms:keysymslst)
    --         | keysym `elem` keysyms = Just n
    --         | otherwise             = findKeycode' (n+1) keysymslst
    --     findKeycode' _ _ = Nothing
