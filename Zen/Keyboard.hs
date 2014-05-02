module Keyboard where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ((\\))
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Data.Time (getZonedTime)
import Graphics.XHB hiding (Setup)
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor
import Graphics.X11.Types (xK_Num_Lock, xK_Caps_Lock)

import Lens
import Util
import Types


keysymToKeycode :: KEYSYM -> KeyboardMap -> Maybe KEYCODE
keysymToKeycode keysym = safeHead . M.keys . M.filter (keysym `elem`)


keycodeToKeysym :: KEYCODE -> KeyboardMap -> [KEYSYM]
keycodeToKeysym keycode = fromMaybe [] . M.lookup keycode


keycodeToModifier :: KEYCODE -> ModifierMap -> Maybe MapIndex
keycodeToModifier keycode = safeHead . M.keys . M.filter (keycode `elem`)


keysymToModifier :: KEYSYM -> KeyboardMap -> ModifierMap -> Maybe MapIndex
keysymToModifier keysym kbdmap modmap =
    keysymToKeycode keysym kbdmap >>= flip keycodeToModifier modmap


modifierToKeycode :: MapIndex -> Map MapIndex [KEYCODE] -> [KEYCODE]
modifierToKeycode = M.findWithDefault []


cleanMask :: KeyboardMap -> ModifierMap -> [KeyButMask] -> [ModMask]
cleanMask kbdmap modmap mask = modifiermask \\ map (fromBit . toValue) modifier
    where
    keycodes = catMaybes [ keysymToKeycode (fi xK_Num_Lock) kbdmap
                         , keysymToKeycode (fi xK_Caps_Lock) kbdmap
                         ]
    modifier = catMaybes $ map (flip keycodeToModifier modmap) $ keycodes
    modifiermask = map (fromBit . toBit) (mask \\ [KeyButMaskButton1 ..])


getCleanMask :: [KeyButMask] -> Z [ModMask]
getCleanMask mask = do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    return $ cleanMask kbdmap modmap mask


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


modifierMapping :: Receipt GetModifierMappingReply -> IO (Map MapIndex [KEYCODE])
modifierMapping receipt = indices <$> getReply receipt
    where
    indices (Left _) = M.empty
    indices (Right reply) =
        let kc_per_mod = fi $ keycodes_per_modifier_GetModifierMappingReply reply
            modifier = partition kc_per_mod $ keycodes_GetModifierMappingReply reply
        in M.fromList $ zip [MapIndexShift ..] modifier



grabModifier :: Connection -> Config -> Setup -> IO ()
grabModifier c conf setup = do
    let modmask = conf ^. modMask
        kbdmap = setup ^. keyboardMap
        modmap = setup ^. modifierMap

        -- TODO: separate function
        nl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Num_Lock) kbdmap modmap]
        cl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Caps_Lock) kbdmap modmap]
        combos m kc = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]
        grab (mask, keycode) = grabKey c $ MkGrabKey True (getRoot c)
                                                     mask keycode
                                                     GrabModeAsync GrabModeAsync

    ungrabKey c $ MkUngrabKey (toValue GrabAny) (getRoot c) [ModMaskAny]

    forM_ modmask $ \mask -> do
        let mapindex = fromValue . toBit $ mask
            keycodes = filter (/= 0) $ modifierToKeycode mapindex modmap
        forM keycodes $ mapM_ grab . combos (mask `L.delete` modmask)
        -- forM keycodes $ \keycode -> do
        --     io . putStrLn $ "grabbing " ++ show (mask `L.delete` modmask) ++ " " ++ show keycode
        --     io . putStrLn $ "combos: " ++ show (combos (mask `L.delete` modmask) keycode)

        --     (io $ putStrLn $ "grabbing " ++ show (mask `L.delete` modmask) ++ " " ++ show k)
        --     (grab (combos (mask `L.delete` modmask) k))
