{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import qualified Data.Map as M (empty)
import Control.Monad.State
import Control.Monad.Reader
import Graphics.XHB

import Core
import Types
import Config

-- TODO:
-- Lenses for data structures
-- Free Monads for Layout
-- Use Word32 for xids & convert to from WINDOW, DRAWABLE, etc.
-- Use motionNotifyEvent_root_x instead of root_x_MotionNotifyEvent

-- IDEAS
-- Use Mod4 with lock after timeout


main :: IO ()
main = connect >>= runLoop

-- http://tronche.com/gui/x/xlib/input/XGetKeyboardMapping.html
-- http://cgit.freedesktop.org/~arnau/xcb-util/tree/keysyms/keysyms.c
-- -> xcb_key_symbols_get_keysym

keyboardMapping :: Connection -> Receipt GetKeyboardMappingReply
                -> IO (Map KEYCODE [KEYSYM])
keyboardMapping c receipt = keycodes' <$> getReply receipt
    where
    keycodes' (Left _) = M.empty
    keycodes' (Right reply) =
        let min_keycode = min_keycode_Setup $ connectionSetup c
            ks_per_kc = fi $ keysyms_per_keycode_GetKeyboardMappingReply reply
            keysyms = partition ks_per_kc $ keysyms_GetKeyboardMappingReply reply
        in M.fromList $ zip [min_keycode ..] keysyms

    partition :: Int -> [a] -> [[a]]
    partition _ [] = []
    partition n lst = take n lst : partition n (drop n lst)

keysymToKeycode :: KEYSYM -> Map KEYCODE [KEYSYM] -> Maybe KEYCODE
keysymToKeycode keysym = safeHead . M.keys . M.filter (fi keysym `elem`)


runLoop :: Maybe Connection -> IO ()
runLoop Nothing = print "Got no connection!"
runLoop (Just c) = runReaderT (evalStateT runZ initialCore) initialSetup
    where
    initialCore = Core initialConfig initialWindowQueue (Position 0 0)
    initialSetup = Setup c (getRoot c) M.empty M.empty
    initialWindowQueue = Queue []
