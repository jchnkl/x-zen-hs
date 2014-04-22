{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

module Setup where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Graphics.XHB hiding (Setup)
import Lens
import Core
import Util hiding (subLists, keyboardMapping, keysymToKeycode)

type WindowXid = WINDOW

data Setup = Setup
    { _connection :: Connection
    , _rootWindow :: WindowXid
    , _keyboardMap :: Map KEYCODE [KEYSYM]
    , _modifierMap :: Map MapIndex [KEYCODE]
    }

type SetupRT = ReaderT Setup

connection :: Lens Setup Connection
connection = lens _connection (\v d -> d { _connection = v })

root :: Lens Setup WindowXid
root = lens _rootWindow (\v d -> d { _rootWindow = v })

keyboardMap :: Lens Setup (Map KEYCODE [KEYSYM])
keyboardMap = lens _keyboardMap (\v d -> d { _keyboardMap = v })

modifierMap :: Lens Setup (Map MapIndex [KEYCODE])
modifierMap = lens _modifierMap (\v d -> d { _modifierMap = v })


initialSetup :: Connection -> Setup
initialSetup c = Setup c (getRoot c) M.empty M.empty


subLists :: Int -> [a] -> [[a]]
subLists _ [] = []
subLists n lst = take n lst : subLists n (drop n lst)


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
            keysyms = subLists ks_per_kc $ keysyms_GetKeyboardMappingReply reply
        in M.fromList $ zip [min_keycode ..] keysyms


keysymToKeycode :: KEYSYM -> M.Map KEYCODE [KEYSYM] -> Maybe KEYCODE
keysymToKeycode keysym = safeHead . M.keys . M.filter (fi keysym `elem`)


-- setKeymap :: Connection -> Setup -> IO Setup
-- setKeymap c setup = do
--     let min_keycode = min_keycode_Setup $ connectionSetup c
--         max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
--     flip (setL keyboardMap) setup <$>
--         (keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode)


-- updateKeymap :: Connection -> Setup -> Maybe MappingNotifyEvent -> IO Setup
-- updateKeymap c setup Nothing = return setup
-- updateKeymap c setup (Just e)
--     | MappingKeyboard == request_MappingNotifyEvent e = setKeymap c setup
--     | otherwise = return setup


-- updateSetup :: Maybe MappingNotifyEvent -> Setup -> IO Setup
-- updateSetup event setup = 
-- updateSetup Nothing setup = do
--     event <- waitForEvent c
--     void $ updateKeymap c setup (fromEvent event)

update :: Maybe MappingNotifyEvent -> Setup -> IO Setup
update Nothing setup = return setup
update (Just e) setup
    | MappingKeyboard == request_MappingNotifyEvent e = do
        let c = setup ^. connection
            min_keycode = min_keycode_Setup $ connectionSetup c
            max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
        flip (setL keyboardMap) setup <$>
            io (keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode)
    | otherwise = return setup

{-
run :: Maybe MappingNotifyEvent -> SetupRT IO a
run Nothing = ask
run (Just e)
    | MappingKeyboard == request_MappingNotifyEvent e = do
        setup <- ask
        let c = setup ^. connection
            min_keycode = min_keycode_Setup $ connectionSetup c
            max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
        flip (setL keyboardMap) setup <$>
            io (keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode)
    | otherwise = ask
-}



runSetup :: Setup -> SomeEvent -> SetupRT IO a -> IO a
runSetup setup event x = update (fromEvent event) setup >>= runReaderT x

-- runSetup _ Nothing      = runReaderT run
-- runSetup c (Just event) = do
--     runReaderT run
--     

-- runSetup :: Connection -> Setup -> IO ()
-- runSetup c setup = do
--     event <- waitForEvent c
--     void $ updateKeymap c setup (fromEvent event)

    -- (foo . bar) $ waitForEvent c

-- runSetup :: Connection -> SetupRT IO ()
-- runSetup c = do
--     let min_keycode = min_keycode_Setup $ connectionSetup c
--         max_keycode = max_keycode_Setup $ connectionSetup c
--     kbdmap <- liftIO (keyboardMapping c
--         =<< getKeyboardMapping c min_keycode (max_keycode - min_keycode + 1))
--     runReaderT (Setup c (getRoot c) kbdmap M.empty)
