-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE
    DeriveDataTypeable,
    DeriveFunctor,
    FlexibleInstances,
    ExistentialQuantification,
    MultiParamTypeClasses,
    StandaloneDeriving,
    RankNTypes
    #-}

module Types where

import Data.Word
import Data.Maybe
import Numeric
import Data.Typeable
import Data.Map (Map)

import Control.Monad.Trans.Free
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Graphics.XHB hiding (Setup)

import Lens


type Log = [String]

type View = Model -> IO ()

type Controller = SetupRT IO AnyEvent


class TypeConversion a b where
    convert :: a -> b


type ControllerComponent = Component (Z IO)


data Component stack = forall d m. (Monad stack, Monad m, Typeable m) => Component
    { -- | Arbitratry id string, for logging only
      componentId :: String
      -- | Component data
    , componentData    :: d
      -- | Evaluation with side effects
    , execComponent   :: m () -> d -> stack d
    -- | Startup hook
    , onStartup        :: d -> stack d
    -- | Shutdown hook
    , onShutdown       :: d -> stack ()
    -- | List of event handlers
    , someHandler        :: d -> [SomeHandler]
    }


runComponent :: Monad core => AnyEvent -> Component core -> core (Component core)
runComponent (AnyEvent e) (Component cid d runio su sd handlers) = do
    d' <- runio (mapM_ (flip dispatch e) (handlers d)) d
    return $ Component cid d' runio su sd handlers


data SomeHandler = forall a. (Typeable a) => SomeHandler a
    deriving Typeable


class Dispatcher event where
    dispatch :: (Typeable handler, Typeable m, Monad m) => handler -> event -> m ()

data AnyEvent = forall a. (Dispatcher a) => AnyEvent a


data EventHandler b = forall a. (Event a) => EventHandler (a -> b)
    deriving (Typeable)


deriving instance Typeable WriterT
deriving instance Typeable ReaderT
deriving instance Typeable StateT
deriving instance Typeable FreeT


instance Dispatcher SomeEvent where
    dispatch sh se = case cast sh of
        Just (EventHandler f) -> case fromEvent se of
            Just e -> f e
            _      -> return ()
        _                     -> return ()



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

    , _views :: [View]
    , _components :: [ControllerComponent]
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

components :: Functor f => LensLike' f Config [ControllerComponent]
components = lens _components (\d v -> d { _components = v })


type WindowId = WINDOW

type Direction = Edge

data Edge = North | South | East | West
    deriving (Eq, Ord, Read, Show, Typeable)

data Position = Position
    { _x :: Int
    , _y :: Int
    }
    deriving (Eq, Ord, Read, Show, Typeable)

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
    deriving (Eq, Ord, Read, Show, Typeable)

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
    deriving (Eq, Ord, Read, Show, Typeable)

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


data Client = Client
    { _xid :: WindowId
    , _pointer :: Position
    , _geometry :: Geometry
    }
    deriving (Eq, Typeable)

instance Show Client where
    show (Client i _ (Geometry pos dim)) =
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


data UpdateHint =
      HintRaise
    | HintLower
    | HintX Int
    | HintY Int
    | HintWidth  Word
    | HintHeight Word
    | HintPosition  Position
    | HintDimension Dimension
    | HintBorderColor Word
    | HintBorderWidth Word
    deriving (Eq, Ord, Show, Typeable)

data Model = Model
    { _queue :: Queue
    }
    deriving (Show, Typeable)

queue :: Functor f => LensLike' f Model Queue
queue = lens _queue (\d v -> d { _queue = v })



data ClientStack = ClientStack
    { _above :: [Client]
    , _focus :: Client
    , _below :: [Client]
    }
    deriving Typeable


data ModelOps f =
      GetQueue (Queue -> f)
    | PutQueue Queue f
    | InsertClient Client f
    | RemoveClient Client f
    | InsertWindow WindowId f
    | RemoveWindow WindowId f
    | Raise     WindowId f
    | Lower     WindowId f
    | SetX      WindowId Int f
    | SetY      WindowId Int f
    | SetWidth  WindowId Word f
    | SetHeight WindowId Word f
    | SetPosition  WindowId Position  f
    | SetDimension WindowId Dimension f
    | SetGeometry  WindowId Geometry f
    | SetBorderColor WindowId Word f
    | SetBorderWidth WindowId Word f
    deriving (Functor, Typeable)


type ModelOpsFT = FreeT ModelOps

type LogWT = WriterT [String]

type ModelST = StateT Model

type SetupRT = ReaderT Setup

type Z m = LogWT (ModelOpsFT (SetupRT m))

