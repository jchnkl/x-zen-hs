{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, RankNTypes, ExistentialQuantification,
   StandaloneDeriving, FlexibleInstances,
   FlexibleContexts,
   MultiParamTypeClasses
   #-}
   -- InstanceSigs

             -- FlexibleContexts,
             -- FlexibleInstances,
             -- MultiParamTypeClasses,
             -- TypeSynonymInstances,
             -- StandaloneDeriving,
             -- GADTs,
             -- RankNTypes,
             -- ConstraintKinds,
             -- TupleSections,
             -- InstanceSigs,
             -- GeneralizedNewtypeDeriving #-}
             -- -- DatatypeContexts,

module Types where
    -- ( module Types
    -- -- , module Lens
    -- ) where

import Data.Word
import Data.Maybe (catMaybes)
import Data.Typeable
import Data.Map (Map)
-- import Control.Arrow (second)
-- import Control.Applicative ((<$>))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB hiding (Setup)

-- import Log
import Lens


class Typeable a => Message a where
    toMessage :: a -> SomeMessage
    toMessage = SomeMessage

    fromMessage :: SomeMessage -> Maybe a
    fromMessage (SomeMessage m) = cast m

data SomeMessage = forall a. Message a => SomeMessage a
    deriving Typeable

instance Message Int

-- data MessageHandler b = forall a . (Message a) => MessageHandler (a -> b)




data ComponentConfig = forall a. (Show a, Typeable a) => ComponentConfig a
    deriving Typeable

-- class ComponentClass c where
--     runComponent_ :: m a -> c -> IO (a, c)
--     startup_ :: c -> Z IO ()
--     startup_ _ = return ()
--     cleanup_ :: c -> Z IO ()
--     cleanup_ _ = return ()
--     eventHandler_ :: forall a m. c -> [a -> Z m ()]

-- data SomeComponent = forall c. ComponentClass c => SomeComponent c

-- data DummyComponent = DummyComponent
--     { cd :: Int
--     , ehs :: forall a m. [a -> Z m ()]
--     }

-- instance ComponentClass DummyComponent where
--     runComponent_ = undefined
--     eventHandler_ (DummyComponent _ hs) = hs

-- dummyEventHandler :: SomeEvent -> Z m ()
-- dummyEventHandler = undefined

-- dummyMessageHandler :: SomeMessage -> Z m ()
-- dummyMessageHandler = undefined

-- dummyComponent = DummyComponent
--     { cd = 1
--     , ehs = [dummyEventHandler, dummyMessageHandler]
--     }

data SomeItem = forall a. Typeable a => SomeItem a

whenJustM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenJustM Nothing _ = return Nothing
whenJustM (Just v) f = liftM Just (f v)

whenJustM_ :: (Functor m, Monad m) => Maybe a -> (a -> m b) -> m ()
whenJustM_ v = void . whenJustM v

class (Typeable a) => Item a where
    toItem :: a -> SomeItem
    toItem = SomeItem

    fromItem :: SomeItem -> Maybe a
    fromItem (SomeItem i) = cast i

    -- call :: (Monad m, Functor m) => (a -> Z m ()) -> SomeItem -> Z m ()
    -- call f i = whenJustM_ (fromItem i) f

data SomeHandler b = forall a. (Handler a) => SomeHandler (a -> b)

class Handler a where
    call_ :: (Monad m) => (a -> Z m ()) -> SomeItem -> Z m ()
    -- call_ f i = whenJustM_ (fromItem i) f

deriving instance Typeable SomeEvent
-- deriving instance Typeable1 (Event a)
-- instance Item SomeEvent

newtype WrappedEvent a = WrapEvent { unwrapEvent :: a }

instance Event a => Handler (WrappedEvent a) where
    -- call_ f item = undefined
    -- call_ f item = whenJustM_ (fromItem item) (flip whenJustM_ f . fromEvent)
    call_ f item = case (fromItem item >>= fromEvent) of
        Nothing -> return ()
        (Just e) -> f $ WrapEvent e

-- instance Handler SomeMessage where
--     call_ f item = undefined

-- instance (Event b) => Handler SomeItem b where
    -- call_ f item = whenJustM_ (fromItem item) (\e -> whenJustM_ (fromEvent e) f)
    -- call_ f item = undefined

-- instance Event b => Handler SomeEvent b where
--     -- call_ :: (Event a, Monad m, Functor m) => (a -> Z m ()) -> b -> Z m ()
--     call_ f se = whenJustM_ (fromEvent se) f

instance Item SomeEvent
    -- call f i = whenJustM_ (fromItem i) (\se -> whenJustM_ (fromEvent se) f)
instance Item SomeMessage

-- instance Event a => Item a where
--     fromItem (SomeItem i) = cast i >>= fromEvent

-- instance Item KeyPressEvent where
--     fromItem (SomeItem i) = cast i >>= fromEvent


data Component = forall m c. (MonadIO m, Typeable c) => Component
    { -- | Component data
      componentData :: c
      -- | Evaluation function
    , runComponent :: forall a. m a -> c -> IO (a, c)
    -- | Function to run on startup
    , startup :: c -> Z IO c
    -- | Function to run on shutdown
    , cleanup :: c -> Z IO ()
    -- | Event handler
    -- , handleEvent :: (SomeEvent -> Z m ())
    -- | Message handler
    -- , handleMessage :: (SomeMessage -> Z m ())
    -- dispatcher :: [Dispatcher (Z m ())]
    -- eventHandler :: [SomeEvent -> Z m ()]
    -- messageHandler :: [SomeMessage -> Z m ()]
    -- , eventHandler :: forall a. (Handler a) => [a -> Z m ()]
    , eventHandler :: [SomeHandler (Z m ())]
    -- , eventHandler :: [SomeEvent -> Z m ()]
    }

-- eventConsumer :: SomeEvent -> IO ()
-- eventConsumer = undefined

-- keyPressConsumer :: KeyPressEvent -> IO ()
-- keyPressConsumer = undefined

-- messageConsumer :: SomeMessage -> IO ()
-- messageConsumer = undefined

-- myConsumer :: [Consumer]
-- myConsumer = [Consumer keyPressConsumer] -- , Consumer messageConsumer]
-- -- myConsumer = [Consumer eventConsumer, Consumer keyPressConsumer] -- , Consumer messageConsumer]

-- runProducer :: Setup -> [Producer] -> IO [(ThreadId, SomeChannel)]
-- runProducer setup = run []
--     where
--     run result [] = return result
--     run result (Producer p : ps) = do
--         tchan <- newBroadcastTChanIO
--         tid <- forkIO $ void $ runReaderT (execWriterT $ p tchan) setup
--         run ((tid, toChannel tchan) : result) ps


{-
connectConsumer :: Setup -> Consumer -> [SomeChannel] -> IO [ThreadId]
connectConsumer setup (Consumer c) =
    fmap catMaybes . mapM (flip whenJustM (forkIO . loop) . fromChannel)

    where
    -- connect [] = return Nothing
    -- connect (somec : somecs) = case (fromChannel somec) of
    --     Nothing -> connect somecs
    --     (Just channel) -> fmap Just (forkIO $ loop channel)

    loop channel = forever $ atomically (readTChan channel)
                                 >>= flip runReaderT setup . execWriterT . c


connectComponent :: Setup -> Component -> [SomeChannel] -> IO [ThreadId]
connectComponent setup (Component d r s c cconsumer) somecs = connect [] cconsumer
    where
    connect tids [] = return tids
    connect tids (c:cs) = do
        ts <- connectConsumer setup c somecs
        connect (ts ++ tids) cs


runConsumer :: Setup -> [TChan SomeItem] -> [Consumer] -> IO ()
runConsumer setup channels cs = 
    atomically (foldr1 orElse $ map readTChan channels) >>= forM_ cs . try

    -- try (c) (fromItem val)
    -- mapM_ (\(Consumer f) -> whenJustM_ (fromItem val) f) cs

    -- tryConsumer cs val


    -- return ()
    where
    -- try _ Nothing     = return ()
    -- try f (Just item) = c item

    -- tryRun Nothing _ = return ()
    try item (Consumer f) = whenJustM_ (fromItem item) f

    -- tryRead Nothing = return Nothing
    -- tryRead (Just chan) = fmap Just $ readTChan chan

    -- tryConsumer (Consumer f : cs) v = f v
    -- tryConsumer _ _ = return ()
    -- tryConsumer (Just v) = fmap Just $ atomically (readTChan chan)

    -- tryRead Nothing = return Nothing
    -- tryRead (Just chan) = fmap Just $ atomically (readTChan chan)

-}


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


{-
class Typeable a => Channel a where
    toChannel :: TChan a -> SomeChannel
    toChannel = SomeChannel

    fromChannel :: SomeChannel -> Maybe (TChan a)
    fromChannel (SomeChannel c) = cast c

deriving instance Typeable SomeEvent
instance Channel SomeEvent
-- instance Channel SomeMessage


-- class Typeable a => Producer a where
--     runProducer :: TChan a -> Z IO ()


-- class Typeable a => Producer a where
--     runProducer :: TChan a -> Z IO ()


data SomeChannel = forall a. Typeable a => SomeChannel (TChan a)


data Producer = forall a. Channel a => Producer (TChan a -> Z IO ())

data SomeItem = forall a. Typeable a => SomeItem a

class (Typeable a) => Item a where
    toItem :: a -> SomeItem
    toItem = SomeItem

    fromItem :: SomeItem -> Maybe a
    fromItem (SomeItem i) = cast i

-- deriving instance Typeable SomeEvent
-- instance Item SomeEvent

instance Event a => Item a where
    fromItem (SomeItem i) = cast i >>= fromEvent

-- instance Item KeyPressEvent where
--     fromItem (SomeItem i) = cast i >>= fromEvent

-- instance Item SomeMessage

data Consumer = forall a. (Item a) => Consumer (a -> IO ())

-}

{-
data SomeObject = forall a. (Object a, Typeable a) => SomeObject a
    deriving Typeable

class Typeable a => Object a where
    toObject :: a -> SomeObject
    toObject = SomeObject

    fromObject :: SomeObject -> Maybe a
    fromObject (SomeObject c) = cast c


instance Object SomeEvent
instance Object SomeMessage


class Typeable a => ChannelClass a where
    toChannel :: TChan a -> Channel
    toChannel = Channel

    fromChannel :: Channel -> Maybe (TChan a)
    fromChannel (Channel c) = cast c

    readChannel :: Channel -> STM (Maybe a)
    readChannel = readc . fromChannel
        where readc Nothing  = return Nothing
              readc (Just c) = fmap Just $ readTChan c

    writeChannel :: Channel -> a -> STM ()
    writeChannel chan = writec (fromChannel chan)
        where writec Nothing  _ = return ()
              writec (Just c) v = writeTChan c v

data Channel = forall a. Typeable a => Channel (TChan a)


instance ChannelClass SomeEvent
instance ChannelClass SomeMessage


class Typeable a => Element a where
    toElement :: a -> SomeElement
    toElement = SomeElement

    fromElement :: SomeElement -> Maybe a
    fromElement (SomeElement e) = cast e


deriving instance Typeable SomeEvent
instance Element SomeEvent
instance Element SomeMessage

data SomeElement = forall a. Element a => SomeElement a
   deriving Typeable
-}


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

    , _eventSources :: [[TMVar Component] -> IO ()]
    -- , _producer :: [Producer]
    -- , _eventQueue :: TChan SomeEvent
    -- , _messageQueue :: TChan SomeMessage
    -- , _eventQueue :: Channel
    -- , _messageQueue :: Channel
    }
    deriving Typeable


runStack :: r -> WriterT w (ReaderT r m) a -> m (a, w)
runStack setup f = runReaderT (runWriterT f) setup

someEventSource :: Setup -> [TMVar Component] -> IO ()
someEventSource setup cvars = do
    -- event <- waitForEvent (setup ^. connection)
    -- mapM_ (withTMVar (dispatch setup event)) cvars
    waitForEvent (setup ^. connection)
        >>= forM_ cvars . withTMVar . dispatch setup . toItem


someMessageSource :: Setup -> [TMVar Component] -> IO ()
someMessageSource setup cvars = do
    -- event <- waitForEvent (setup ^. connection)
    -- mapM_ (withTMVar (dispatch setup event)) cvars
    -- waitForEvent (setup ^. connection) >>= forM_ cvars . withTMVar . dispatch setup
    forM_ cvars . withTMVar . dispatch setup . toItem $ SomeMessage (0::Int)


withTMVar :: (a -> IO a) -> TMVar a -> IO ()
withTMVar f var = atomically (takeTMVar var) >>= f >>= atomically . putTMVar var


dispatch :: Setup -> SomeItem -> Component -> IO Component
dispatch setup event (Component cdata runc s c hs) = do
    cdata' <- run runc event cdata hs
    return (Component cdata' runc s c hs)

    where

    -- run :: forall a m c. (m a -> c -> IO (a, c))
    --     -> SomeItem
    --     -> a
    --     -> [SomeHandler (Z m ())]
    --     -> m a
    run _ e d []       = return d
    run r e d (SomeHandler f : fs) = do
        -- let d' = undefined
        ((_, logs), d') <- r (runStack setup (call_ f e)) d
        -- printLog logs
        run r e d' fs

runComponents :: Setup -> IO ()
runComponents setup = do
    cvars <- mapM newTMVarIO cs

    -- tids <- mapM newTMVarIO cs >>= mapM forkIO . flip map sources . ($)

    tids <- mapM forkIO $ map ($ cvars) sources
    -- tids <- mapM forkIO $ map ($ cvars) $ map ($ setup) sources
    -- mapM forkIO $ map ($ cvars) $ map ($ setup) sources

    -- forM_ (_eventSources setup) (forkIO . ($ cvars))
    forM_ ([someEventSource setup, someMessageSource setup]) (forkIO . ($ cvars))

    where
    cs = setup ^. config .components
    sources= [someEventSource setup, someMessageSource setup]

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

-- messageQueue :: Functor f => LensLike' f Setup Channel
-- messageQueue = lens _messageQueue (\d v -> d { _messageQueue = v })

type LogWT = WriterT [String]

-- type MessageWT = WriterT [SomeMessage]

type SetupRT = ReaderT Setup

type Z m a = LogWT (SetupRT m) a

type StatelessZ a = Z IO a
