-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, GADTs, MultiParamTypeClasses, RankNTypes #-}

module Types where

import Data.Word
import Data.Typeable
import Data.Map (Map)

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent.STM (TMVar)

import Graphics.XHB hiding (Setup)

import Lens


class TypeConversion a b where
    convert :: a -> b


class (Typeable a) => Reply a where
    toReply :: Maybe a -> SomeReply
    toReply Nothing = NoReply
    toReply (Just v) = SomeReply v

    fromReply :: SomeReply -> Maybe a
    fromReply NoReply = Nothing
    fromReply (SomeReply v) = cast v


class (Typeable a) => Message a where
    toMessage :: a -> SomeMessage
    toMessage = SomeMessage

    fromMessage :: SomeMessage -> Maybe a
    fromMessage (SomeMessage m) = cast m


data SomeReply where
    NoReply :: SomeReply
    SomeReply :: (Reply a) => a -> SomeReply
    deriving Typeable


data SomeMessage where
    SomeMessage :: (Message a) => a -> SomeMessage
    deriving Typeable


data MessageCom where
    MessageCom :: TMVar SomeReply -> SomeMessage -> MessageCom
    deriving Typeable


instance Sink MessageCom where
    dispatch com (MessageHandler f) = f com
    dispatch _ _                    = return ()


data CoreMessage where
    -- IsClientReply
    IsClient      :: WindowId -> CoreMessage

    -- GetClientReply
    GetClient     :: WindowId -> CoreMessage
    -- GetClientsReply
    GetQueue      :: CoreMessage

    -- WithClientReply
    WithClient    :: WindowId -> (Client -> a) -> CoreMessage

    -- WithClientsReply
    WithQueue     :: (Queue -> a) -> CoreMessage

    -- VoidReply
    ModifyClient  :: WindowId -> (Client -> Client) -> CoreMessage

    -- VoidReply
    ModifyQueue   :: (Queue -> Queue) -> CoreMessage
    deriving (Typeable)

instance Message CoreMessage


data CoreMessageReply where
    VoidReply       :: CoreMessageReply

    IsClientReply   :: { isClientReply :: Bool } -> CoreMessageReply

    GetClientReply  :: { getClientReply :: Maybe Client } -> CoreMessageReply

    GetQueueReply   :: { getQueueReply :: Queue } -> CoreMessageReply

    WithClientReply :: { withClientReply :: Maybe a } -> CoreMessageReply

    WithQueueReply  :: { withQueueReply :: a } -> CoreMessageReply
    deriving (Typeable)

instance Reply CoreMessageReply


data ComponentConfig = forall a. Typeable a => ComponentConfig a
    deriving Typeable


class Sink a where
    dispatch :: forall m. (MonadIO m, Functor m) => a -> SomeSink (m ()) -> m ()






data SomeSource where
    EventSource :: (Setup -> IO SomeEvent) -> SomeSource
    MessageSource :: (Setup -> IO SomeMessage) -> SomeSource


data SomeSink b where
    EventHandler :: Event a => (a -> b) -> SomeSink b
    MessageHandler :: (MessageCom -> b) -> SomeSink b


data Component = forall m d. (MonadIO m, Functor m, Typeable d) => Component
    { -- | Component data
      componentData :: d
      -- | Evaluation function
    , runComponent :: forall a. m a -> d -> IO (a, d)
    -- | Function to run on startup
    , onStartup :: d -> Z IO d
    -- | Function to run on shutdown
    , onShutdown :: d -> Z IO ()
    -- | Generic event handler
    , someSinks :: d -> [SomeSink (Z m ())]
    }





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

   -- this should be a own component for reloading on KeyboardMappingNotify
   -- provide message service for {un,}grabbing keys
    , _keyboardMap :: KeyboardMap
    , _modifierMap :: ModifierMap

    -- , _producer :: [Producer]
    -- , _eventQueue :: TChan SomeEvent
    -- , _messageQueue :: TChan SomeMessage
    -- , _eventQueue :: Channel
    -- , _messageQueue :: Channel
    -- , _eventSources :: [SomeChannel]
    -- , _eventChannels :: [EventSource TChan]
    -- , _messageQueue :: [TChan SomeData]
    }
    deriving Typeable

config :: Functor f => LensLike' f Setup Config
config = lens _config (\d v -> d { _config = v })

connection :: Functor f => LensLike' f Setup Connection
connection = lens _connection (\d v -> d { _connection = v })

rootWindow :: Functor f => LensLike' f Setup WindowId
rootWindow = lens _rootWindow (\d v -> d { _rootWindow = v })

-- buttonMask :: Functor f => LensLike' f Setup [EventMask]
-- buttonMask = lens _buttonMask (\d v -> d { _buttonMask = v })

keyboardMap :: Functor f => LensLike' f Setup (Map KEYCODE [KEYSYM])
keyboardMap = lens _keyboardMap (\d v -> d { _keyboardMap = v })

modifierMap :: Functor f => LensLike' f Setup (Map MapIndex [KEYCODE])
modifierMap = lens _modifierMap (\d v -> d { _modifierMap = v })

-- eventQueue :: Functor f => LensLike' f Setup Channel
-- eventQueue = lens _eventQueue (\d v -> d { _eventQueue = v })

-- messageQueue :: Functor f => LensLike' f Setup [TChan SomeData]
-- messageQueue = lens _messageQueue (\d v -> d { _messageQueue = v })

type LogWT = WriterT [String]

-- data HandlerManageOps
--     = AttachHandler Int SomeHandler
--     | DetachHandler Int

-- type HandlerWT = WriterT [HandlerManageOps]

-- instance Hashable SomeHandler where
--     hashWithSalt salt (SomeHandler _ f) = hashWithSalt salt (typeOf f)

-- attachHandler :: Monad m => SomeHandler -> Z m Int
-- attachHandler h = do
--     lift $ tell [AttachHandler hashsum h]
--     return hashsum
--     where hashsum = hash h

-- detachHandler :: Monad m => Int -> Z m ()
-- detachHandler i = lift . tell $ [DetachHandler i]

-- type MessageWT = WriterT [SomeMessage]

type SetupRT = ReaderT Setup

-- type Z m a = LogWT (HandlerWT (SetupRT m)) a
type Z m a = SetupRT (LogWT m) a

type StatelessZ a = Z IO a
