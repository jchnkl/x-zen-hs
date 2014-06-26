-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE
   DeriveDataTypeable,
   FlexibleInstances,
   GADTs,
   MultiParamTypeClasses,
   RankNTypes,
   TypeSynonymInstances
   #-}

module Types where

import Data.Word
import Data.Maybe
import Numeric
import Data.Typeable
import Data.Map (Map)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Graphics.XHB hiding (Setup)

import Lens


type View = Model -> IO ()


class TypeConversion a b where
    convert :: a -> b


data Component = forall d m. (MonadIO m, Functor m) => Component
    { -- | Arbitratry id string, for logging only
      componentId :: String
      -- | Component data
    , componentData    :: d
      -- | Evaluation with side effects
    , ioRunComponent   :: forall a. m a -> d -> IO (a, d)
    -- | Startup hook
    , onStartup        :: d -> Z IO d
    -- | Shutdown hook
    , onShutdown       :: d -> Z IO ()
    -- | List of event handlers
    , someHandler        :: d -> [SomeHandler]
    }


class Typeable a => Handler a where
    fromHandler :: SomeHandler -> Maybe a
    fromHandler (SomeHandler h) = cast h

data SomeHandler = forall a. Handler a => SomeHandler a
    deriving Typeable


class Dispatcher a where
    dispatch :: MonadIO m => a -> SomeHandler -> Z m ()

data AnyEvent = forall a. Dispatcher a => AnyEvent a


data EventHandler b = forall a. Event a => EventHandler (a -> b)
    deriving (Typeable)

instance Typeable1 (Z m) where
   typeOf1 _ = mkTyConApp (mkTyCon3 "zen" "Zen.Types" "Z") []

instance Handler (EventHandler (Z m ()))


instance Dispatcher SomeEvent where
    dispatch se sh = case fromHandler sh :: Maybe (EventHandler (Z m ())) of
        Just (EventHandler f) -> case fromEvent se of
            Just e -> f e
            _ -> return ()
        _ -> return ()


type EventSource m = Setup -> IO AnyEvent


-- TODO:
-- * separate components for Key/Button grabbing
-- * message KeyGrab / ButtonGrab
-- * border stuff -> Core

data Config = Config
    { _modMask :: [ModMask]
    , _borderWidth :: Word
    , _normalBorderColor :: Word
    , _focusedBorderColor :: Word
    , _selectionBorderColor :: Word

    , _components :: [Component]
    }
    deriving (Typeable)

modMask :: Functor f => LensLike' f Config [ModMask]
modMask f d = (\v -> d { _modMask = v }) `fmap` (f (_modMask d))

borderWidth :: Functor f => LensLike' f Config Word
borderWidth f d = (\v -> d { _borderWidth = v }) `fmap` (f (_borderWidth d))

normalBorderColor :: Functor f => LensLike' f Config Word
normalBorderColor = lens _normalBorderColor (\d v -> d { _normalBorderColor = v })

focusedBorderColor :: Functor f => LensLike' f Config Word
focusedBorderColor = lens _focusedBorderColor (\d v -> d { _focusedBorderColor = v })

selectionBorderColor :: Functor f => LensLike' f Config Word
selectionBorderColor = lens _selectionBorderColor (\d v -> d { _selectionBorderColor = v })

components :: Functor f => LensLike' f Config [Component]
components = lens _components (\d v -> d { _components = v })


type WindowId = WINDOW

type Direction = Edge

data Edge = North | South | East | West
    deriving (Eq, Ord, Read, Show, Typeable)

data Position = Position
    { _x :: Int
    , _y :: Int
    }
    deriving (Eq, Read, Show, Typeable)

x :: Functor f => LensLike' f Position Int
x = lens _x (\d v -> d { _x = v })

y :: Functor f => LensLike' f Position Int
y = lens _y (\d v -> d { _y = v })

nullPosition :: Position
nullPosition = Position 0 0

instance Num Position where
    Position lx ly + Position rx ry = Position (lx + rx) (ly + ry)
    Position lx ly - Position rx ry = Position (lx - rx) (ly - ry)
    Position lx ly * Position rx ry = Position (lx * rx) (ly * ry)
    abs (Position x' y') = Position (abs x') (abs y')
    signum (Position x' y') = Position (signum x') (signum y')
    fromInteger i = Position (fromInteger i) (fromInteger i)


data Dimension = Dimension
    { _width :: Word
    , _height :: Word
    }
    deriving (Eq, Read, Show, Typeable)

width :: Functor f => LensLike' f Dimension Word
width = lens _width (\d v -> d { _width = v })

height :: Functor f => LensLike' f Dimension Word
height = lens _height (\d v -> d { _height = v })

nullDimension :: Dimension
nullDimension = Dimension 0 0

instance Num Dimension where
    Dimension lw lh + Dimension rw rh = Dimension (lw + rw) (lh + rh)
    Dimension lw lh - Dimension rw rh = Dimension (lw - rw) (lh - rh)
    Dimension lw lh * Dimension rw rh = Dimension (lw * rw) (lh * rh)
    abs (Dimension w' h') = Dimension (abs w') (abs h')
    signum (Dimension w' h') = Dimension (signum w') (signum h')
    fromInteger i = Dimension (fromInteger i) (fromInteger i)


data Geometry = Geometry
    { _position :: Position
    , _dimension :: Dimension
    }
    deriving (Eq, Read, Show, Typeable)

position :: Functor f => LensLike' f Geometry Position
position = lens _position (\d v -> d { _position = v })

dimension :: Functor f => LensLike' f Geometry Dimension
dimension = lens _dimension (\d v -> d { _dimension = v })

nullGeometry :: Geometry
nullGeometry = Geometry nullPosition nullDimension

instance Num Geometry where
    Geometry lp ld + Geometry rp rd = Geometry (lp + rp) (ld + rd)
    Geometry lp ld - Geometry rp rd = Geometry (lp - rp) (ld - rd)
    Geometry lp ld * Geometry rp rd = Geometry (lp * rp) (ld * rd)
    abs (Geometry p' d') = Geometry (abs p') (abs d')
    signum (Geometry p' d') = Geometry (signum p') (signum d')
    fromInteger i = Geometry (fromInteger i) (fromInteger i)


data UpdateHint = UpdateX
                | UpdateY
                | UpdateWidth
                | UpdateHeight
                | UpdateBorderWidth
                | UpdateBorderColor
   deriving (Eq, Typeable)

data Client = Client
    { _xid         :: WindowId
    , _pointer     :: Position
    , _geometry    :: Geometry
    , _updateHints :: [UpdateHint]
    }
    deriving (Eq, Typeable)

instance Show Client where
    show (Client i _ (Geometry pos dim) _) =
        "0x" ++ showHex (fromXid $ toXid i :: Word32) ""
        ++ " @ " ++ show (pos ^. x) ++ "x" ++ show (pos ^. y) ++ "+"
                 ++ show (dim ^. width) ++ "+" ++ show (dim ^. height)

xid :: Functor f => LensLike' f Client WindowId
xid = lens _xid (\d v -> d { _xid = v })

pointer :: Functor f => LensLike' f Client Position
pointer = lens _pointer (\d v -> d { _pointer = v })

geometry :: Functor f => LensLike' f Client Geometry
geometry = lens _geometry (\d v -> d { _geometry = v })


type Queue = ClientQueue

data ClientQueue = ClientQueue
    { above :: [Client]
    , focus :: Maybe Client
    , below :: [Client]
    }
    deriving (Typeable)

instance Show ClientQueue where
    show = unlines . map show . toList
       where toList (ClientQueue as mc bs) = maybeToList mc ++ as ++ bs


type KeyboardMap = Map KEYCODE [KEYSYM]
type ModifierMap = Map MapIndex [KEYCODE]

data Setup = Setup
    { _config :: Config

    , _connection :: Connection
    , _rootWindow :: WindowId

   -- this should be a own component for reloading on KeyboardMappingNotify
   -- provide message service for {un,}grabbing keys
    , _keyboardMap :: KeyboardMap
    , _modifierMap :: ModifierMap
    }
    deriving Typeable

config :: Functor f => LensLike' f Setup Config
config = lens _config (\d v -> d { _config = v })

connection :: Functor f => LensLike' f Setup Connection
connection = lens _connection (\d v -> d { _connection = v })

rootWindow :: Functor f => LensLike' f Setup WindowId
rootWindow = lens _rootWindow (\d v -> d { _rootWindow = v })

keyboardMap :: Functor f => LensLike' f Setup (Map KEYCODE [KEYSYM])
keyboardMap = lens _keyboardMap (\d v -> d { _keyboardMap = v })

modifierMap :: Functor f => LensLike' f Setup (Map MapIndex [KEYCODE])
modifierMap = lens _modifierMap (\d v -> d { _modifierMap = v })


type Controller = SetupRT IO AnyEvent

type Model = ClientQueue
type ModelST = StateT Model

data ClientStack = ClientStack
    { _above :: [Client]
    , _focus :: Client
    , _below :: [Client]
    }
    deriving Typeable


type LogWT = WriterT [String]

type SetupRT = ReaderT Setup

type Z m = LogWT (ModelST (SetupRT m))
