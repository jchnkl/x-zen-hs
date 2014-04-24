{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-# LANGUAGE ExistentialQuantification #-}

module Types
    ( module Types
    , module Lens
    ) where

import Data.Word
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Graphics.XHB hiding (Setup)
import Graphics.X11.Types (KeySym)

import Lens

data EventHandler b = forall a . Event a => EventHandler (a -> b)

data KeyEventHandler = KeyEventHandler
    { keyPress :: KeyPressEvent -> Z ()
    , keyRelease :: KeyReleaseEvent -> Z ()
    }

type KeyHandler = Map ([ModMask], KeySym) KeyEventHandler

data ButtonEventHandler = ButtonEventHandler
    { buttonPress :: ButtonPressEvent -> Z ()
    , buttonRelease :: ButtonReleaseEvent -> Z ()
    }

type ButtonHandler = Map ([ModMask], ButtonIndex) ButtonEventHandler

data Config = Config
    { _modMask :: [ModMask]
    , _borderWidth :: Word
    , _normalBorderColor :: Word
    , _focusedBorderColor :: Word
    , _selectionBorderColor :: Word
    , _keyHandler :: KeyHandler
    , _buttonHandler :: ButtonHandler
    }

modMask :: Lens Config [ModMask]
modMask = lens _modMask (\v d -> d { _modMask = v })

borderWidth :: Lens Config Word
borderWidth = lens _borderWidth (\v d -> d { _borderWidth = v })

normalBorderColor :: Lens Config Word
normalBorderColor = lens _normalBorderColor (\v d -> d { _normalBorderColor = v })

focusedBorderColor :: Lens Config Word
focusedBorderColor = lens _focusedBorderColor (\v d -> d { _focusedBorderColor = v })

selectionBorderColor :: Lens Config Word
selectionBorderColor = lens _selectionBorderColor (\v d -> d { _selectionBorderColor = v })

keyHandler :: Lens Config KeyHandler
keyHandler = lens _keyHandler (\v d -> d { _keyHandler = v })

buttonHandler :: Lens Config ButtonHandler
buttonHandler = lens _buttonHandler (\v d -> d { _buttonHandler = v })


type WindowId = WINDOW

data Position = Position
    { _x :: Int
    , _y :: Int
    }
    deriving (Eq, Read, Show)

x :: Lens Position Int
x = lens _x (\v d -> d { _x = v })

y :: Lens Position Int
y = lens _y (\v d -> d { _y = v })


data Dimension = Dimension
    { _width :: Word
    , _height :: Word
    }
    deriving (Eq, Read, Show)

width :: Lens Dimension Word
width = lens _width (\v d -> d { _width = v })

height :: Lens Dimension Word
height = lens _height (\v d -> d { _height = v })


data Geometry = Geometry
    { _position :: Position
    , _dimension :: Dimension
    }
    deriving (Eq, Read, Show)

position :: Lens Geometry Position
position = lens _position (\v d -> d { _position = v })

dimension :: Lens Geometry Dimension
dimension = lens _dimension (\v d -> d { _dimension = v })


data Client = Client
    { _xid :: WindowId
    , _pointer :: Position
    }
    deriving (Eq, Show)

xid :: Lens Client WindowId
xid = lens _xid (\v d -> d { _xid = v })

pointer :: Lens Client Position
pointer = lens _pointer (\v d -> d { _pointer = v })


data Queue = Queue
    { _above :: [Client]
    , _focus :: Maybe Client
    , _below :: [Client]
    }

above :: Lens Queue [Client]
above = lens _above (\v d -> d { _above = v })

focus :: Lens Queue (Maybe Client)
focus = lens _focus (\v d -> d { _focus = v })

below :: Lens Queue [Client]
below = lens _below (\v d -> d { _below = v })


data Core = Core
    { _queue :: Queue
    , _eventHandler :: [EventHandler (Z ())]
    }

queue :: Lens Core Queue
queue = lens _queue (\v d -> d { _queue = v })

eventHandler :: Lens Core [EventHandler (Z ())]
eventHandler = lens _eventHandler (\v d -> d { _eventHandler = v })


data Setup = Setup
    { _config :: Config
    , _connection :: Connection
    , _rootWindow :: WindowId
    , _keyboardMap :: Map KEYCODE [KEYSYM]
    , _modifierMap :: Map MapIndex [KEYCODE]
    }

config :: Lens Setup Config
config = lens _config (\v d -> d { _config = v })

connection :: Lens Setup Connection
connection = lens _connection (\v d -> d { _connection = v })

rootWindow :: Lens Setup WindowId
rootWindow = lens _rootWindow (\v d -> d { _rootWindow = v })

keyboardMap :: Lens Setup (Map KEYCODE [KEYSYM])
keyboardMap = lens _keyboardMap (\v d -> d { _keyboardMap = v })

modifierMap :: Lens Setup (Map MapIndex [KEYCODE])
modifierMap = lens _modifierMap (\v d -> d { _modifierMap = v })


type LogWT = WriterT [String]

type CoreST = StateT Core

type SetupRT = ReaderT Setup

type Z = LogWT (CoreST (SetupRT IO))
