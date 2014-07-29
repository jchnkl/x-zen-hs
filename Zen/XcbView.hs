{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

module XcbView where

import Data.Maybe (catMaybes)
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Typeable
import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans (lift)
import Graphics.XHB (EventMask(..), ConfigWindow(..), ButtonIndex(..),
                     ModMask(..), GrabButton(..), GrabKey(..), GrabMode(..),
                     UngrabKey(..), UngrabButton(..))
import qualified Graphics.XHB as X
import Graphics.X11 (KeySym, xK_Num_Lock, xK_Caps_Lock)

import Log
import Lens
import Util
import Types
import qualified Keyboard as K

data XcbView = XcbView
    deriving Typeable

type XcbViewStack = ViewStack


buttonEventMask :: [EventMask]
buttonEventMask =
    [ EventMaskButtonMotion
    , EventMaskButtonPress
    , EventMaskButtonRelease
    ]


xcbView :: ViewComponent
xcbView = Component
    { componentId = "XcbView"
    , componentData = XcbView
    , execComponent = \f d -> f >> return d
    , onStartup = return . id
    , onShutdown = const $ return ()
    , someHandler = const $
        L.map SomeHandler [ ModelHandler modelPrinter ]
        ++
        L.map SomeHandler [ ClientConfigHandler clientConfigHandler ]
    }


modelPrinter :: Model -> XcbViewStack ()
modelPrinter = toLog . show


clientConfigHandler :: ClientConfigs -> XcbViewStack ()
clientConfigHandler = mapM_ (flip runStateT [] . uncurry configureClient)
                    . M.toList . M.map S.toList
    where
    configure w vp = connection $-> \c -> io $ X.configureWindow c w vp

    configureClient w []           = gets X.toValueParam >>= configure w
    configureClient w (conf:confs) = do doConfigureClient w conf
                                        configureClient w confs

    doConfigureClient w = \case
        ConfigClientX      v     -> modify ((ConfigWindowX, fi v):)
        ConfigClientY      v     -> modify ((ConfigWindowY, fi v):)
        ConfigClientWidth  v     -> modify ((ConfigWindowWidth, fi v):)
        ConfigClientHeight v     -> modify ((ConfigWindowHeight, fi v):)
        ConfigGrabKey      ks mm -> grabKey w ks mm
        ConfigUngrabKey    ks mm -> ungrabKey w ks mm
        ConfigGrabButton   bi mm -> grabButton w bi mm
        ConfigUngrabButton bi mm -> ungrabButton w bi mm


grabKey, ungrabKey :: (MonadReader Setup m, Functor m, MonadIO m)
                   => WindowId -> KeySym -> [ModMask] -> m ()
grabKey window keysym keymask = do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    modmask <- askL (config . modMask)

    let specialKeys ks = catMaybes [(X.fromBit . X.toValue)
                                    <$> K.keysymToModifier kbdmap modmap (fi ks)]
        nl = specialKeys xK_Num_Lock
        cl = specialKeys xK_Caps_Lock
        -- TODO: separate function
        combos m kc = L.nub $ zip (m : L.map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]

        grab (mask, keycode) = connection $-> \c -> io $ X.grabKey c $
            MkGrabKey True window mask keycode GrabModeAsync GrabModeAsync

    whenJustM_ (K.keysymToKeycode kbdmap (fi keysym)) $
        mapM_ grab . combos (modmask ++ keymask)

ungrabKey window keysym keymask = do
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap
    modmask <- askL (config . modMask)

    let specialKeys ks = catMaybes [(X.fromBit . X.toValue)
                                    <$> K.keysymToModifier kbdmap modmap (fi ks)]
        nl = specialKeys xK_Num_Lock
        cl = specialKeys xK_Caps_Lock
        -- TODO: separate function
        combos m kc = L.nub $ zip (m : L.map (m ++) [nl, cl, nl ++ cl]) [kc, kc ..]

        ungrab (mask, keycode) = connection $-> \c -> io $ X.ungrabKey c $
            MkUngrabKey keycode window mask

    whenJustM_ (K.keysymToKeycode kbdmap (fi keysym)) $
        mapM_ ungrab . combos (modmask ++ keymask)


grabButton, ungrabButton :: (MonadReader Setup m, MonadIO m)
                         => WindowId -> ButtonIndex -> [ModMask] -> m ()
grabButton window buttonidx buttonmask = do
    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap

    let combos = (combinations (buttonmask ++ modmask ++ K.extraModifier kbdmap modmap))
    mapM_ (grab window buttonEventMask) $ zip combos $ repeat buttonidx

    where
    grab w emask (mask, button) = connection $-> \c -> do
        io $ X.grabButton c $ MkGrabButton True w emask
                              GrabModeAsync GrabModeAsync
                              (convertXid X.xidNone) (convertXid X.xidNone)
                              button mask

ungrabButton window buttonidx buttonmask = do
    modmask <- askL (config . modMask)
    kbdmap <- askL keyboardMap
    modmap <- askL modifierMap

    let combos = (combinations (buttonmask ++ modmask ++ K.extraModifier kbdmap modmap))
        ungrab (mask, button) = connection $-> \c -> do
            io $ X.ungrabButton c $ MkUngrabButton button window mask

    mapM_ ungrab $ zip combos $ repeat buttonidx
