{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, RankNTypes, ExistentialQuantification #-}

module Types where
    -- ( module Types
    -- , module Lens
    -- ) where

import Data.Word
import Data.Typeable
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Graphics.XHB hiding (Setup)

-- import Log
import Lens


data SomeMessage = forall a. (Typeable a) => SomeMessage a
    deriving Typeable


data ComponentConfig = forall a. (Show a, Typeable a) => ComponentConfig a
    deriving Typeable


data Component = forall m c. (MonadIO m, Typeable c) => Component
    { -- | Component data
      component :: c
      -- | Evaluation function
    , runComponent :: (forall a. m a -> c -> IO (a, c))
    -- | Function to run on startup
    , startup :: c -> Z IO c
    -- | Function to run on shutdown
    , cleanup :: c -> Z IO ()
    -- | Event handler
    , handleEvent :: (SomeEvent -> Z m ())
    -- | Message handler
    , handleMessage :: (SomeMessage -> Z m ())
    }


data EventHandler b = forall a . (Event a) => EventHandler (a -> b)

data Config = Config
    { _modMask :: [ModMask]
    , _borderWidth :: Word
    , _normalBorderColor :: Word
    , _focusedBorderColor :: Word
    , _selectionBorderColor :: Word
    , _components :: [Component]
    , _componentConfigs :: [ComponentConfig]
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

componentConfigs :: Functor f => LensLike' f Config [ComponentConfig]
componentConfigs = lens _componentConfigs (\d v -> d { _componentConfigs = v })


type WindowId = WINDOW

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

data Mode = Normal | Manage
    deriving (Eq, Read, Show, Typeable)

data Core = Core
    { _mode :: Mode
    , _queue :: Queue
    }
    deriving (Show, Typeable)

mode :: Functor f => LensLike' f Core Mode
mode = lens _mode (\d v -> d { _mode = v })

queue :: Functor f => LensLike' f Core Queue
queue = lens _queue (\d v -> d { _queue = v })

type KeyboardMap = Map KEYCODE [KEYSYM]
type ModifierMap = Map MapIndex [KEYCODE]

data Setup = Setup
    { _config :: Config

    , _connection :: Connection
    , _rootWindow :: WindowId

    , _keyboardMap :: KeyboardMap
    , _modifierMap :: ModifierMap

    , _eventQueue :: TChan SomeEvent
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

type LogWT = WriterT [String]

type MessageWT = WriterT [SomeMessage]

type SetupRT = ReaderT Setup

type Z m a = LogWT (MessageWT (SetupRT m)) a

type StatelessZ a = Z IO a
