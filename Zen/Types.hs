{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification,
             FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Types where
    -- ( module Types
    -- , module Lens
    -- ) where

import Data.Word
import Data.Typeable
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Graphics.XHB hiding (Setup)
import Graphics.X11.Types (KeySym)
import Graphics.X11.Xlib.Font (Glyph)

-- import Lens
import Lens.Family
-- import Lens.Family.State
import Lens.Family.Unchecked

data EventHandler b = forall a . (Typeable (a -> b), Event a) => EventHandler (a -> b)
    deriving Typeable

instance Eq (EventHandler a) where
    EventHandler l == EventHandler r = typeOf l == typeOf r

instance Ord (EventHandler a) where
    EventHandler l `compare` EventHandler r = typeOf l `compare` typeOf r


data InputEventHandler pe re = InputHandler
    { press   :: pe -> Z ()
    , release :: re -> Z ()
    }

-- mkInputHandler :: Maybe (pe -> Z ()) -> Maybe (re -> Z ()) -> InputEventHandler pe re
mkInputHandler :: InputEventHandler pe re
mkInputHandler = InputHandler (const $ return ()) (const $ return ())

type KeyEventHandler = InputEventHandler KeyPressEvent KeyReleaseEvent
type ButtonEventHandler = InputEventHandler ButtonPressEvent ButtonReleaseEvent

-- data KeyEventHandler = KeyEventHandler
--     { keyPress :: KeyPressEvent -> Z ()
--     , keyRelease :: KeyReleaseEvent -> Z ()
--     }

-- type KeyHandler = Map ([ModMask], KeySym) KeyEventHandler
type KeyHandler = Map ([ModMask], KeySym) KeyEventHandler

-- data ButtonEventHandler = ButtonEventHandler
--     { buttonPress :: ButtonPressEvent -> Z ()
--     , buttonRelease :: ButtonReleaseEvent -> Z ()
--     }

-- type ButtonHandler = Map ([ModMask], ButtonIndex) ButtonEventHandler
type ButtonHandler = Map ([ModMask], ButtonIndex) ButtonEventHandler

data Config = Config
    { _modMask :: [ModMask]
    , _borderWidth :: Word
    , _normalBorderColor :: Word
    , _focusedBorderColor :: Word
    , _selectionBorderColor :: Word
    , _keyHandler :: KeyHandler
    -- , _buttonHandler :: ButtonHandler
    , _buttonHandler :: ButtonHandler
    }

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

keyHandler :: Functor f => LensLike' f Config KeyHandler
keyHandler = lens _keyHandler (\d v -> d { _keyHandler = v })

buttonHandler :: Functor f => LensLike' f Config ButtonHandler
buttonHandler = lens _buttonHandler (\d v -> d { _buttonHandler = v })


type WindowId = WINDOW

data Edge = None | North | South | East | West
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


data Dimension = Dimension
    { _width :: Word
    , _height :: Word
    }
    deriving (Eq, Read, Show, Typeable)

width :: Functor f => LensLike' f Dimension Word
width = lens _width (\d v -> d { _width = v })

height :: Functor f => LensLike' f Dimension Word
height = lens _height (\d v -> d { _height = v })


data Geometry = Geometry
    { _position :: Position
    , _dimension :: Dimension
    }
    deriving (Eq, Read, Show, Typeable)

position :: Functor f => LensLike' f Geometry Position
position = lens _position (\d v -> d { _position = v })

dimension :: Functor f => LensLike' f Geometry Dimension
dimension = lens _dimension (\d v -> d { _dimension = v })


data Client = Client
    { _xid :: WindowId
    , _geometry :: Geometry
    , _pointer :: Position
    }
    deriving (Eq, Show, Typeable)

xid :: Functor f => LensLike' f Client WindowId
xid = lens _xid (\d v -> d { _xid = v })

geometry :: Functor f => LensLike' f Client Geometry
geometry = lens _geometry (\d v -> d { _geometry = v })

pointer :: Functor f => LensLike' f Client Position
pointer = lens _pointer (\d v -> d { _pointer = v })


type Queue = Map WindowId Client

-- data Queue = Queue
--     { _above :: [Client]
--     , _focus :: Maybe Client
--     , _below :: [Client]
--     }

-- above :: Lens Queue [Client]
-- above = lens _above (\v d -> d { _above = v })

-- focus :: Lens Queue (Maybe Client)
-- focus = lens _focus (\v d -> d { _focus = v })

-- below :: Lens Queue [Client]
-- below = lens _below (\v d -> d { _below = v })

type EventHooks = Set (EventHandler (Z ()))

data Core = Core
    { _queue :: Queue
    , _eventHooks :: EventHooks
    }
    deriving Typeable

queue :: Functor f => LensLike' f Core Queue
queue = lens _queue (\d v -> d { _queue = v })

eventHooks :: Functor f => LensLike' f Core EventHooks
eventHooks = lens _eventHooks (\d v -> d { _eventHooks = v })

data Setup = Setup
    { _config :: Config
    , _connection :: Connection
    , _rootWindow :: WindowId
    , _keyboardMap :: Map KEYCODE [KEYSYM]
    , _modifierMap :: Map MapIndex [KEYCODE]
    , _glyphCursors :: Map Glyph CURSOR
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

glyphCursors :: Functor f => LensLike' f Setup (Map Glyph CURSOR)
glyphCursors = lens _glyphCursors (\d v -> d { _glyphCursors = v })


type LogWT = WriterT [String]

type CoreST = StateT Core

type SetupRT = ReaderT Setup

newtype Z a = Z (LogWT (CoreST (SetupRT IO)) a)
    deriving ( Applicative
             , Functor
             , Monad
             , MonadIO
             , MonadReader Setup
             , MonadState Core
             , MonadWriter [String]
             , Typeable
             )
