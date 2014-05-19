{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

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
import Graphics.XHB hiding (Setup)
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor
import Graphics.X11.Types (KeySym, xK_Num_Lock, xK_Caps_Lock)

import Lens
import Util
import Types


class TypeConversion a b where
    convert :: a -> b

instance TypeConversion [KeyButMask] [ModMask] where
    convert = map (fromBit . toBit) . (\\ [KeyButMaskButton1 ..])

instance TypeConversion [ModMask] [KeyButMask] where
    convert = map (fromBit . toBit) . (\\ [ModMaskAny])

instance TypeConversion [MapIndex] [ModMask] where
    convert = map (fromBit . toValue)

instance TypeConversion [ModMask] [MapIndex] where
    convert = map (fromValue . toBit) . (\\ [ModMaskAny])

specialKeys :: [KeySym]
specialKeys = [xK_Num_Lock, xK_Caps_Lock]

keysymToKeycode :: KeyboardMap -> KEYSYM -> Maybe KEYCODE
keysymToKeycode kbdmap = safeHead . M.keys . flip M.filter kbdmap . elem


keycodeToKeysym :: KeyboardMap -> KEYCODE -> [KEYSYM]
keycodeToKeysym kbdmap = fromMaybe [] . flip M.lookup kbdmap


keycodeToModifier :: ModifierMap -> KEYCODE -> Maybe MapIndex
keycodeToModifier modmap = safeHead . M.keys . flip M.filter modmap . elem


keysymToModifier :: KeyboardMap -> ModifierMap -> KEYSYM -> Maybe MapIndex
keysymToModifier kbdmap modmap keysym =
    keysymToKeycode kbdmap keysym >>= keycodeToModifier modmap


modifierToKeycode :: ModifierMap -> MapIndex -> [KEYCODE]
modifierToKeycode = flip (M.findWithDefault [])


cleanMask :: TypeConversion [a] [ModMask]
          => KeyboardMap -> ModifierMap -> [a] -> [ModMask]
cleanMask kbdmap modmap mask = (convert mask) \\ map (fromBit . toValue) modifier
    where
    keycodes = catMaybes $ map (keysymToKeycode kbdmap . fi) specialKeys
    modifier = catMaybes $ map (keycodeToModifier modmap) $ keycodes


getCleanMask :: Monad m => TypeConversion [a] [ModMask] => [a] -> Z m [ModMask]
getCleanMask mask = do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    return $ cleanMask kbdmap modmap mask


extraModifier :: KeyboardMap -> ModifierMap -> [ModMask]
extraModifier kbdmap modmap =
    convert $ catMaybes $ map (keysymToModifier kbdmap modmap . fi) specialKeys


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

        modifier = [] : combinations (extraModifier kbdmap modmap)
        keycodes = filter (/= 0)
                 $ concatMap (modifierToKeycode modmap) (convert modmask)
        grab (mask, keycode) = grabKey c $ MkGrabKey True (getRoot c)
                                                     mask keycode
                                                     GrabModeAsync GrabModeAsync

    ungrabKey c $ MkUngrabKey (toValue GrabAny) (getRoot c) [ModMaskAny]

    mapM_ grab $ concatMap (zip modifier . repeat) keycodes

{-
grabKeys :: Connection -> Config -> Setup -> IO ()
grabKeys c conf setup = do
    let modmask = conf ^. modMask
        kbdmap = setup ^. keyboardMap
        modmap = setup ^. modifierMap
        keys = M.keys (conf ^. keyHandler)
        nl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Num_Lock) kbdmap modmap]
        cl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Caps_Lock) kbdmap modmap]
        -- TODO: separate function
        combos m kc = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]
        grab (mask, keycode) = grabKey c $ MkGrabKey True (getRoot c)
                                                     mask keycode
                                                     GrabModeAsync GrabModeAsync

    ungrabKey c $ MkUngrabKey (toValue GrabAny) (getRoot c) [ModMaskAny]

    forM_ keys $ \(mask, keysym) ->
        whenJust (keysymToKeycode (fi keysym) kbdmap) $
            mapM_ grab . combos (modmask ++ mask)
-}
