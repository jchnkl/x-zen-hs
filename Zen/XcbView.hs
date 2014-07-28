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
                     ModMask(..), GrabButton(..), GrabKey(..), GrabMode(..))
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
        ConfigClientX      _ v _     -> modify ((ConfigWindowX, fi v):)
        ConfigClientY      _ v _     -> modify ((ConfigWindowY, fi v):)
        ConfigClientWidth  _ v _     -> modify ((ConfigWindowWidth, fi v):)
        ConfigClientHeight _ v _     -> modify ((ConfigWindowHeight, fi v):)
        ConfigGrabKey      _ ks mm _ -> grabKey w ks mm
        ConfigGrabButton   _ bi mm _ -> grabButton w bi mm


grabKey :: (MonadReader Setup m, Functor m, MonadIO m)
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


grabButton :: (MonadReader Setup m, MonadIO m)
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
