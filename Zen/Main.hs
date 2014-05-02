{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import Data.Time (getZonedTime)
import Graphics.XHB hiding (Setup)
import Graphics.X11.Xlib.Font (Glyph)
import Graphics.X11.Xlib.Cursor
import Graphics.X11.Types hiding (Connection, EventMask)

import Log
import Util
import Lens
-- import Core
import Types hiding (config)
-- import Config
-- import Setup hiding (config)
import Event
import Window
import Cursor
import Pointer

-- TODO:
-- Free Monads for Layout
-- Split in proper modules
--   Pointer -> grabButton, *Cursor, etc.
--   Keyboard -> grabKeys, etc.
--   or generic X module?

-- IDEAS
-- Use Mod4 with lock after timeout
-- data BorderColor = BorderColor { _normal :: Word | _focused :: Word | etc.


-- | Cursors to be loaded at program startup
-- Cursors are available through @_glyphCursors@ in @Setup@
cursorGlyphs :: [Glyph]
cursorGlyphs =
    [ xC_fleur
    , xC_top_side
    , xC_top_right_corner
    , xC_top_left_corner
    , xC_bottom_side
    , xC_bottom_right_corner
    , xC_bottom_left_corner
    , xC_right_side
    , xC_left_side
    , xC_sizing
    ]

eventMaskButton :: [EventMask]
eventMaskButton =
    [ EventMaskButtonMotion
    , EventMaskButtonPress
    , EventMaskButtonRelease
    ]

config :: Config
config = Config
    { _modMask = [ModMask1]
    , _borderWidth = 3
    , _normalBorderColor = 0x00a0a0a0
    , _focusedBorderColor = 0x00ffce28
    , _selectionBorderColor = 0x00ff0000

    , _keyHandler = M.fromList
        [ (([], xK_a), InputHandler
            { press = \_ -> (toLog ("[], xK_a" ))
            , release = \_ -> (toLog ("[], xK_a"))
            } )

        , (([ModMaskShift], xK_a), InputHandler
            { press = \_ -> (toLog ("[ModMaskShift], xK_a" ))
            , release = \_ -> (toLog ("[ModMaskShift], xK_a"))
            } )
        ]

    , _buttonHandler = M.fromList
        [ (([], ButtonIndex1), moveWindowHandler)
        , (([], ButtonIndex2), resizeWindowHandler)
        , (([], ButtonIndex3), lowerWindowHandler)
        , (([ModMaskShift], ButtonIndex3), raiseWindowHandler)
        ]
    }


main :: IO ()
main = connect >>= startup

startup :: Maybe Connection -> IO ()
startup Nothing = print "Got no connection!"
startup (Just c) = do
    let mask = CWEventMask
        values = toMask [EventMaskSubstructureRedirect, EventMaskSubstructureNotify, EventMaskFocusChange]
        valueparam = toValueParam [(mask, values)]
    changeWindowAttributes c (getRoot c) valueparam

    withSetup c $ \setup -> do
        -- TODO: ungrab / regrab keys for MappingNotifyEvent
        -- grabKeys c config setup
        grabModifier c config setup

        run setup . snd
            =<< runCore setup (Core Normal M.empty S.empty) . mapM_ manage
                =<< children <$> (queryTree c (getRoot c) >>= getReply)

    where
    run :: Setup -> Core -> IO ()
    run setup core' = do
        (logstr, core'') <- runCore setup core' runZ
        time <- getZonedTime
        putStrLn . (show time ++) . ("\n" ++) . unlines . map ("\t" ++) $ logstr
        run setup core''

    runZ :: Z ()
    runZ = connection $-> io . waitForEvent >>= dispatch

    runCore :: Setup -> Core -> Z () -> IO ([String], Core)
    runCore setup core (Z z) = runReaderT (runStateT (execWriterT z) core) setup

    children :: Either SomeError QueryTreeReply -> [WindowId]
    children (Left _) = []
    children (Right reply) = children_QueryTreeReply reply


withSetup :: Connection -> (Setup -> IO a) -> IO a
withSetup c f = withFont c "cursor" $ \font -> do
    withGlyphCursors c font cursorGlyphs $ \cursors -> do
        let min_keycode = min_keycode_Setup $ connectionSetup c
            max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
        kbdmap <- keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode
        modmap <- modifierMapping =<< getModifierMapping c
        f $ Setup config c (getRoot c) eventMaskButton kbdmap modmap cursors


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
