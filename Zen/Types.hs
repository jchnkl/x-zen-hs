-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable,
             ExistentialQuantification,
             RankNTypes
     , MultiParamTypeClasses
    -- , TypeFamilies
    -- , ConstraintKinds
    , TupleSections
    -- , StandaloneDeriving
    , TypeSynonymInstances
    , FlexibleInstances
    -- , FlexibleContexts
    #-}
-- {-# LANGUAGE DeriveDataTypeable, RankNTypes, ExistentialQuantification
--     , FlexibleInstances
--     , FunctionalDependencies

module Types
    ( module Types
    , module Message
    ) where

import GHC.Exts (Constraint)
import Data.Word
import Data.Hashable
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.Cont
import Control.Arrow (second)
import Control.Applicative
-- import Control.Monad

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

-- import Control.Monad.Trans
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Trans.Writer
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB hiding (Setup)

-- import Log
import Lens
import Lens.Family.Stock
import Message -- hiding (dispatch)

-- class Typeable a => EventWrapper a where
--     wrapEvent :: a -> SomeEventWrapper
--     wrapEvent = SomeEventWrapper
--     unwrapEvent :: SomeEventWrapper -> Maybe a
--     unwrapEvent (SomeEventWrapper e) = cast e

-- data SomeEventWrapper = forall a. (EventWrapper a) => SomeEventWrapper a
-- data SomeHandler b = forall a. (Message a) => SomeHandler (a -> b)

-- deriving instance Typeable (Z m ())
-- deriving instance Typeable Z

-- class (Typeable a) => Handler a where
--     toHandler :: forall b. Typeable b => (a -> b) -> SomeHandler
--     toHandler = SomeHandler
--     fromHandler :: forall b. Typeable b => SomeHandler -> Maybe (a -> b)
--     fromHandler (SomeHandler f) = cast f

-- data HandlerId = HandlerId { getHandlerId :: ThreadId }
--     deriving (Show, Typeable)

-- data SomeChannel = forall a. Channel a => SomeChannel (TChan a)

-- class Typeable a => Channel a where
--     toChannel :: TChan a -> SomeChannel
--     toChannel = SomeChannel
--     fromChannel :: SomeChannel -> Maybe (TChan a)
--     fromChannel (SomeChannel c) = cast c


-- data EventSource = forall e. Channel e => EventSource (IO e)


-- runSources :: [EventSource] -> IO [(ThreadId, SomeChannel)]
-- runSources = mapM run
--     where
--     run :: EventSource -> IO (ThreadId, SomeChannel)
--     run (EventSource f) = do
--         chan <- newBroadcastTChanIO
--         tid <- forkIO $ forever $ f >>= atomically . writeTChan chan
--         return (tid, toChannel chan)


-- whenJustM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
-- whenJustM Nothing _ = return Nothing
-- whenJustM (Just v) f = liftM Just (f v)


-- attach :: (Monad m, Functor m) => Handler -> Z m (Maybe HandlerId)
-- attach :: Handler -> Z IO (Maybe HandlerId)
-- attach (Handler h) = do
--     channels <- asks _eventSources

    -- let channel' = fromChannel $ head channels
    -- ask >>= flip try channel'

    -- return Nothing

    -- where
    -- try setup = flip whenJustM (liftIO . forkIO . run setup)

    -- run setup channel = atomically (dupTChan channel)
    --     >>= forever . (runStack setup . h =<<) . atomically . readTChan

    -- runStack :: ComponentClass d m => Setup -> (Z m a) -> IO ()
    -- runStack = undefined
    -- dc = DummyComponent 1


-- detach :: MonadIO m => HandlerId -> Z m ()
-- detach = liftIO . killThread . getHandlerId


-- runComponents :: Setup -> IO ()
-- runComponents setup = do
--
--     -- let f = undefined :: Z m ()
--     mapM_ run cs
--
--     where
--         cs = setup ^. config . components
--
--         run (Component cdata runc s c f hs) = do
--             ((_, logs), cdata') <- runc (runStack setup f) cdata
--             return (Component cdata' runc s c f hs)




-- class EventHandler_ e d where
--     handle :: d -> e -> IO () -- Z m ()
--     -- runComponent_ :: SomeMessage -> d -> IO ()

data ComponentConfig = forall a. (Show a, Typeable a) => ComponentConfig a
    deriving Typeable


-- type EventHandlerRegistry =
--     -- forall a m. (Typeable a, Monad m) => Map ThreadId (a -> Z m ())
--     forall a m.  Map ThreadId (a -> Z m ())


-- data ComponentStore = ComponentStore
--    { eventHandler :: EventHandlerRegistry
--    , component :: Component
--    }

-- type ComponentStoreST = StateT ComponentStore

-- type Y = SetupRT (ComponentStoreST IO)

-- foo :: Y ()
-- foo = do
--     h <- gets (M.lookup (undefined :: ThreadId) . eventHandler)
--     liftIO $ print "foo"

xwhenJustM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
xwhenJustM Nothing _ = return Nothing
xwhenJustM (Just v) f = liftM Just (f v)

xwhenJustM_ :: (Functor m, Monad m) => Maybe a -> (a -> m b) -> m ()
xwhenJustM_ v = void . xwhenJustM v


data Component = forall m d. (Monad m, Functor m, Typeable d) => Component
    { -- | Component data
      componentData :: d
      -- | Evaluation function
    , runComponent :: forall a. m a -> d -> IO (a, d)
    -- | Function to run on startup
    , onStartup :: d -> Z IO d
    -- | Function to run on shutdown
    , onShutdown :: d -> Z IO ()
    -- -- | Update Component after every dispatched event
    -- , postHandler :: Maybe ([r] -> Component -> Z IO Component)
    -- | Event handler
    -- , eventHandler :: d -> [SomeHandler (Z m ())]
    , someHandler :: [SomeHandler_ (Z m ())]
    }


data ComponentEventHandler = ComponentEventHandler
    { eventHandler :: forall m. MonadIO m => EventHandler (Z m ())
    }

data ComponentMessageHandler = ComponentMessageHandler
    { messageHandler :: forall m. MonadIO m => [MessageHandler (Z m ())]
    }

data Component_ = Component_
    Component
    ComponentEventHandler
    ComponentMessageHandler

class ComponentEventHandler_ a where
    getEventHandler :: forall m. MonadIO m => a -> EventHandler (Z m ())

instance ComponentEventHandler_ ComponentEventHandler where
    getEventHandler (ComponentEventHandler f) = f


-- class Typeable e => SomeHandlerClass e where
--     fromHandler :: SomeHandler_ -> Maybe (e -> m ())
--     -- fromHandler (SomeHandler_ f) = cast f
--     -- eventHandler_ :: forall m. MonadIO m => Maybe (a -> EventHandler (Z m ()))
--     -- messageHandler_ :: forall m. MonadIO m => Maybe (a -> MessageHandler (Z m ()))

data SomeHandler_ b
    = forall a. Event a => EventHandler_ (a -> b)
    | forall a. Message a => MessageHandler_ (a -> b)


-- foo :: Setup
--     -> SomeEvent
--     -> Component
--     -- -> ContT () (WriterT [String] (ReaderT Setup m)) SomeEvent
--     -> ContT () IO SomeEvent
-- foo setup event (Component cdata runc u d) = ContT $ \k -> do
--     runc (runStack setup $ k event) cdata
--     return ()
--     -- k event

-- bar :: Connection -> IO () -- Either SomeError (IO ())
-- bar c = do
--     _ <- fmap (baz =<<) . getReply
--         =<< internAtom c (MkInternAtom False atom_name_len atom_name)
--     return ()
--     where
--     atom_name = stringToCList "foo"
--     atom_name_len = fromIntegral $ length atom_name
--     baz :: ATOM -> (Either SomeError String)
--     baz = undefined

-- data SomeComponentHandler
--     = forall h. EventHandler h => ComponentEventHandler h
--     | forall h. MessageHandler h => ComponentMessageHandler h

class Dispatcher a where
    dispatch :: forall m. (Monad m, Functor m) => a -> SomeHandler_ (m ()) -> m ()

instance Dispatcher SomeEvent where
    dispatch e (EventHandler_ f) = xwhenJustM_ (fromEvent e) f
    dispatch e _                 = return ()

foo :: ButtonPressEvent -> Z IO ()
foo = undefined

bar :: ButtonReleaseEvent -> Z IO ()
bar = undefined

data Dummy = Dummy deriving Typeable
instance Message Dummy

baz :: Dummy -> Z IO ()
baz = undefined

foobarbaz = [EventHandler_ foo, MessageHandler_ baz, EventHandler_ bar]

myComponent :: Component
myComponent = Component
    { componentData = (1 :: Int)
    , runComponent = \f i -> (,i) <$> f
    , onStartup = const $ return 1
    , onShutdown = const $ return ()
    , someHandler = foobarbaz
    }

yoyoyo :: Setup -> SomeEvent -> IO Component
yoyoyo setup event = dispatchEvent setup event myComponent

dispatchEvent :: Dispatcher a => Setup -> a -> Component -> IO Component
dispatchEvent setup event (Component cdata runc u d hs) = do
    ((_, logs), cdata') <- runc (runStack setup $ mapM_ (dispatch event) hs) cdata
    -- printLog logs
    return $ Component cdata' runc u d hs


runStack :: Setup -> WriterT w (ReaderT Setup m) a -> m (a, w)
runStack setup f = runReaderT (runWriterT f) setup

-- myEventHandler :: Component -> [SomeHandler (Z m r)]
-- myEventHandler (Component cdata r u d h) = []


-- execComponent :: Setup -> SomeData -> Component -> IO Component
-- execComponent setup somed component@(Component cdata runc s c ehandler) = do
--     -- exec cdata [] (ehandler component) >>= update . second catMaybes
--     fmap (\(d, _) -> Component d runc s c ehandler) $ exec cdata [] (ehandler component)
--     --printLog logs

    -- where

    -- update (d, rs) = do
    --     (component', logs) <- runStack setup
    --                         $ updatec rs (Component d runc s c ehandler)
    --     -- printLog logs
    --     return component'

    -- exec d rs []     = return (d, rs)
    -- exec d rs (h:hs) = do
    --     ((r, logs), d') <- runc (runStack setup $ try h) d
    --     -- printLog logs
    --     exec d (r:rs) hs

    -- try (SomeHandler conv f) = xwhenJustM (fromData somed >>= conv) f


-- class (Typeable e) => HandlerClass e where
--     execHandler :: Handler (Z m ()) -> e -> Z m ()

-- deriving instance Typeable SomeEvent
-- instance HandlerClass SomeEvent where
--     execHandler (Handler h) e = h $ fromJust (fromEvent e)

-- attachHandler :: (HandlerClass e, Monad m) => Handler -> Z m ()
-- attachHandler :: (Monad m) => Handler (Z m ()) -> Z m ()
-- attachHandler handler@(Handler h) = do
--     setup <- ask
--     cs <- asks (_components . _config)
--     return ()
--
--     where
--     run setup comp@(Component cdata runc s c f hs) = do
--         let event = undefined :: SomeEvent
--         ((_, logs), cdata') <- runc (runStack setup (execHandler handler event)) cdata
--         return (Component cdata' runc s c f hs)


data EventHandler b = forall a. (Event a) => EventHandler (a -> b)
data MessageHandler b = forall a. (Message a) => MessageHandler (a -> b)

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
    , _messageQueue :: [TChan SomeData]
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

messageQueue :: Functor f => LensLike' f Setup [TChan SomeData]
messageQueue = lens _messageQueue (\d v -> d { _messageQueue = v })

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
type Z m a = LogWT (SetupRT m) a

type StatelessZ a = Z IO a
