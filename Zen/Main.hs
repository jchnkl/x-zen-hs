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
import Graphics.X11.Types hiding (Connection, keyPress, keyRelease, buttonPress, buttonRelease)

import Log
import Util
import Lens
-- import Core
import Types hiding (config)
-- import Config
-- import Setup hiding (config)
import Event
import Window

-- TODO:
-- Lenses for data structures
-- Free Monads for Layout
-- Use Word32 for xids & convert to from WINDOW, DRAWABLE, etc.
-- Use motionNotifyEvent_root_x instead of root_x_MotionNotifyEvent

-- IDEAS
-- Use Mod4 with lock after timeout
-- data BorderColor = BorderColor { _normal :: Word | _focused :: Word | etc.


config :: Config
config = Config
    { _modMask = [ModMask1]
    , _borderWidth = 3
    , _normalBorderColor = 0x00a0a0a0
    , _focusedBorderColor = 0x00ffce28
    , _selectionBorderColor = 0x00ff0000

    , _keyHandler = M.fromList
        [ (([], xK_a), InputHandler
            { press = io . print
            , release = io . print
            } )

        , (([ModMaskShift], xK_a), InputHandler
            { press = io . print
            , release = io . print
            } )
        ]

    , _buttonHandler = M.fromList
        [ (([], ButtonIndex1), InputHandler
            { press = \e -> do
                toLog "Press ButtonIndex1"
                let window = event_ButtonPressEvent e
                    event_x = event_x_ButtonPressEvent e
                    event_y = event_y_ButtonPressEvent e
                    pos = Position (fi event_x) (fi event_y)

                raise window
                pushHandler $ EventHandler $ moveWindow $ Just pos

            , release = const $ popHandler $ EventHandler $ moveWindow Nothing
            }
          )

        , (([], ButtonIndex2), mkInputHandler
            { press = \e -> do
                toLog "Press ButtonIndex2"
                let window = event_ButtonPressEvent e
                    root_x = root_x_ButtonPressEvent e
                    root_y = root_y_ButtonPressEvent e
                    w' = fi . width_GetGeometryReply
                    h' = fi . height_GetGeometryReply
                    pos = Position (fi root_x) (fi root_y)
                    dim g = Dimension (w' g) (h' g)

                raise window

                reply' <- io . getReply =<<
                    (io . flip getGeometry (convertXid window)) <-$ connection

                void $ whenRight reply' $ \reply ->
                    pushHandler $ EventHandler $ resizeWindow $ Just (pos, (dim reply))

              , release = const $ popHandler $ EventHandler $ resizeWindow Nothing
              }
          )

        , (([], ButtonIndex3), mkInputHandler
            { press = lower . event_ButtonPressEvent }
          )

        , (([ModMaskShift], ButtonIndex3), mkInputHandler
            { press = raise . event_ButtonPressEvent }
          )

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

    setup <- makeSetup c
    -- TODO: ungrab / regrab keys for MappingNotifyEvent
    grabKeys c config setup

    run setup
        =<< execCore setup (Core M.empty S.empty) . mapM_ manage
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

    execCore :: Setup -> Core -> Z () -> IO Core
    execCore setup core (Z z) = runReaderT (execStateT (execWriterT z) core) setup

    children :: Either SomeError QueryTreeReply -> [WindowId]
    children (Left _) = []
    children (Right reply) = children_QueryTreeReply reply


makeSetup :: Connection -> IO Setup
makeSetup c = do
    let min_keycode = min_keycode_Setup $ connectionSetup c
        max_keycode = max_keycode_Setup (connectionSetup c) - min_keycode + 1
    kbdmap <- keyboardMapping c =<< getKeyboardMapping c min_keycode max_keycode
    modmap <- modifierMapping =<< getModifierMapping c
    return $ Setup config c (getRoot c) kbdmap modmap


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


grabKeys :: Connection -> Config -> Setup -> IO ()
grabKeys c conf setup = do
    let modmask = conf ^. modMask
        kbdmap = setup ^. keyboardMap
        modmap = setup ^. modifierMap
        keys = M.keys (conf ^. keyHandler)
        nl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Num_Lock) kbdmap modmap]
        cl = catMaybes [(fromBit . toValue) <$> keysymToModifier (fi xK_Caps_Lock) kbdmap modmap]
        combos m kc = L.nub $ zip (m : map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]
        grab (mask, keycode) = grabKey c $ MkGrabKey True (getRoot c)
                                                     mask keycode
                                                     GrabModeAsync GrabModeAsync

    ungrabKey c $ MkUngrabKey (toValue GrabAny) (getRoot c) [ModMaskAny]

    forM_ keys $ \(mask, keysym) ->
        whenJust (keysymToKeycode (fi keysym) kbdmap) $
            mapM_ grab . combos (modmask ++ mask)
