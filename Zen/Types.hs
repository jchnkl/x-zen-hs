{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable,
             ExistentialQuantification,
             FlexibleContexts,
             FlexibleInstances,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             StandaloneDeriving,
             GADTs,
             RankNTypes,
             ConstraintKinds,
             GeneralizedNewtypeDeriving #-}

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
import Graphics.X11.Xlib.Font (Glyph)

-- import Log
import Lens
import Lens.Family.Unchecked (lens)

printLog = print . show

data ComponentConfig = forall a. (Show a, Typeable a) => ComponentConfig a
    deriving Typeable

deriving instance Show ComponentConfig

data SomeMessage = forall a. (Typeable a) => SomeMessage a
    deriving Typeable

-- deriving instance Show SomeMessage



{-
class (MonadTrans t, Typeable s) => Acomponent t s where
    -- init :: Z m s
    -- component :: Z (t s m) ()
    -- component :: Z (t IO) ()
    -- execComponent :: StateT String IO () -> s -> m s
    execComponent :: t IO a -> s -> IO s
    -- execStateT :: Monad m => StateT s m a -> s -> m s
    -- handleEvent :: SomeEvent -> Z m ()
    -- eventHandler :: SomeEvent -> Z m ()

acRun :: Z (StateT AC IO) ()
acRun = undefined

data AC = AC String
    deriving Typeable

data BC = BC Int
    deriving Typeable

type Foo = StateT AC
type Bar = StateT BC
type FooBar m a = Foo (Bar m) a

instance Acomponent Bar BC where
    execComponent f s = execStateT undefined (execStateT f s) undefined

instance Acomponent (StateT AC) AC where
    execComponent = execStateT

data ComponentState
    = forall t s. (Monad (t IO), MonadTrans t, Acomponent t s) =>
        ComponentState s (Z (t IO) ())
    |   ComponentWithoutState (Z (IO) ())

runComponents :: Setup -> [ComponentState] -> IO [ComponentState]
runComponents setup = run []
    where
    run :: [ComponentState] -> [ComponentState] -> IO [ComponentState]
    run result (ComponentState s f : components) = do
        s' <- execComponent (runReaderT (execWriterT (execWriterT f)) setup) s
        run (ComponentState s' f : result) components
    run result (ComponentWithoutState f : components) = do
        -- _ <- (runReaderT (execWriterT f) setup)
        _ <- (runReaderT (execWriterT (execWriterT f)) setup)
        run (ComponentWithoutState f : result) components
    run result _ = return result

foo :: [ComponentState]
foo = [ComponentState (AC "ac") acRun]
-}




sendMessage :: Typeable s => SomeMessage -> Z (StateT s IO) ()
sendMessage msg = do
    cs <- askL (config . components)
    -- forM_ cs $ send msg
    -- res <- send [] cs
    -- putL (config . components) res
    return ()
    -- where

-- -- send :: MonadIO m => SomeMessage -> [Component] -> [Component] ->
-- -- Z m [Component]
-- send msg result ((Stateful i s h r) : rest) = do
--     -- s' <- execStateT (runReaderT (execWriterT (r msg)) undefined) s
--     s' <- execStateT (r msg) s
--     send msg ((Stateful i s' h r) : result) rest
-- send msg result _ = return result


-- class SimpleComponent where
--     simple_handleEvent :: SomeEvent -> Z IO ()
--     simple_handleEvent _ = return ()
--
--     simple_receiveMessage :: SomeMessage -> Z IO ()
--     simple_receiveMessage _ = return ()

-- class ComponentClass s where
--     runComponent :: MonadIO m => m a -> s -> IO (a, s)

-- class (MonadIO m, MonadTrans t) => EventHandlerClass t s m where
-- class MonadIO (t s IO) => EventHandlerClass t s where
--     handleEvent :: MonadIO (t s IO) => SomeEvent -> Z (t s IO) ()

class ComponentClass s where
    -- runComponent :: t s m a -> s -> m (a, s)
    -- -- runComponent = runStateT
    -- eventHandler :: SomeEvent -> Z (StateT s m) ()
    -- messageHandler :: SomeMessage -> Z (StateT s m) ()


data Component m s = StateComponent
    { componentState :: s
    , runComponent :: forall a. (Typeable a) => m a -> s -> IO (a, s)
    -- , runComponent :: forall a. a => m a -> s -> IO (a, s)
    , eventHandler :: SomeEvent -> Z m ()
    , messageHandler :: SomeMessage -> Z m ()
    }

data SomeComponent = forall s m. (Monad m) => SomeComponent (Component m s)

foo :: Component (StateT String IO) String
foo = StateComponent
    ("")
    runStateT
    (\_ -> return ())
    (\_ -> return ())

bar :: Component (StateT Int IO) Int
bar = StateComponent
    (1::Int)
    runStateT
    (\_ -> return ())
    (\_ -> return ())

foobar = [SomeComponent foo, SomeComponent bar]

baz :: IO [SomeComponent]
baz = runComponents undefined foobar undefined

-- class ComponentClass s where
--     -- componentState :: Z (StateT s IO) a
--     -- componentState :: Monad (t s IO) => Z (t s IO) a
--     -- runComponent :: MonadIO m => m a -> s -> IO (a, s)
--     runComponent :: t s IO a -> s -> IO (a, s)
--     eventHandler :: SomeEvent -> Z (StateT s IO) ()
--     messageHandler :: SomeMessage -> Z (StateT s IO) ()


-- class MonadIO m => ComponentHandler m where
--     eventHandler :: SomeEvent -> Z m ()
--     messageHandler :: SomeMessage -> Z m ()


-- data Cfoo where
--     MkCfoo :: forall m s a b. (MonadIO m, ComponentClass s) =>
--         s -> (Z m ()) -> (m (a, b) -> s -> IO ((a, b), s)) -> Cfoo

-- something :: IO () -> Int -> IO ((), Int)
-- something = undefined

-- cfoos :: Setup -> [Cfoo] -> IO [Cfoo]
-- cfoos setup (MkCfoo c f r : _) = do
--     ((_, _), _) <- r (runStack setup f) c
--     return []

-- data Foo m s a = Foo
--     { foo_State :: s
--     -- , foo_Function :: forall m. (MonadIO m) => m a
--     , foo_Function :: forall a. (forall m s. StateT s m a) => StateT s m a
--     , foo_RunState :: StateT s m a -> s -> m (a, s)
--     }
--     -- deriving Typeable2
--     -- deriving Typeable2

-- deriving instance Typeable1 m => Typeable (Foo m s a)

-- foo_bar :: StateT Int IO ()
-- foo_bar = undefined

-- foo :: Foo (StateT Int IO) Int ()
-- foo :: Foo Int ()
-- foo = Foo (1::Int) foo_bar runStateT

-- runFoo :: Foo Int () -> IO ()
-- runFoo (Foo s f rf) = do
--     (_,_) <- rf f s
--     return ()

-- data FooWrapper = forall a. (Typeable a) => FooWrapper a
-- data FooWrapper2 = forall a. (forall s. StateT s IO a) => FooWrapper2 a
-- data FooWrapper3 = forall a s. StateT s IO a => FooWrapper3 a

-- foos :: [FooWrapper]
-- foos = [FooWrapper foo]

-- data Component = Dummy
--                -- | forall m. MonadIO m => Simple
--                | forall m. MonadIO m => Simple
--                     (SomeEvent -> Z m ())
--                     (SomeMessage -> Z m ())

               -- | forall s m. (MonadIO m, ComponentClass s) => Complex
               --      s
               --      (SomeEvent -> Z m ())
               --      (SomeMessage -> Z m ())


--                | forall a s m. (ComponentClass s) => Complex
--                     -- s
--                     -- (SomeEvent -> Z m ())
--                     -- (SomeMessage -> Z m ())
--
--                     { complex_State :: s
--                     , complex_Run :: (m a -> s -> IO (a, s))
--                     -- , complex_Run :: StateT s IO ([String], [SomeMessage]) -> s -> IO (([String], [SomeMessage]), s)
--                     , complex_HandleEvent :: (SomeEvent -> Z m ())
--                     , complex_HandleMessage :: Int -- (SomeMessage -> Z (m s) ())
--                     }

-- data Component -- = SimpleComponent => SimpleComponent
--                -- = forall s. StateTComponent s => StateTComponent s
--                -- = forall s m. (Monad m, Foo s m) => Bar s (Z m ())
--                = Simple (SomeEvent -> Z IO ())
--                | forall m s. (MonadIO m, ComponentClass s) =>
--                     Complex s (SomeEvent -> Z m ())
--                     -- Complex s (SomeEvent -> Z m ())

-- runStack :: (Monad m, Monoid w)
--          => r -> WriterT a (WriterT w (ReaderT r m)) a1 -> m (a, w)
runStack setup f = runReaderT (runWriterT (execWriterT f)) setup

runComponents :: Setup -> [SomeComponent] -> SomeEvent -> IO [SomeComponent]
runComponents setup states event = run [] states
    where
    run result [] = return result
    run result (SomeComponent (StateComponent s r e m) : rest) = do
        ((logs, msgs), cstate') <- r (runStack setup (e event)) s
    -- run result (Foo cstate : rest) = do
        -- ((logs, msgs), cstate') <- runComponent (runStack setup (eventHandler event)) cstate
        -- ((logs, msgs), cstate') <- runComponent (eventHandler event) cstate
        -- runComponent (runStack setup (componentState)) cstate
    -- run result (Complex cstate frun fevent fmessage : rest) = do
    -- run result (Complex cstate : rest) = do
        -- ((logs, msgs), cstate') <- runComponent (runStack setup (eventHandler event)) cstate
        -- ((logs, msgs), cstate') <- frun (runStack setup (fevent event)) cstate
        -- ((logs, msgs), cstate'') <- runComponent (runStack setup (fmessage undefined)) cstate'
        printLog logs

        run (SomeComponent (StateComponent cstate' r e m) : result) rest
        -- return []

    -- run result (StateTComponent s : rest) = do
    --     ((logs, msgs), s') <- statet_runComponent (runStack setup (statet_handleEvent event)) s
    --     -- printLog logs
    --     run (StateTComponent s : result) rest


    run result (s : somestates) = do
        run (s : result) somestates
    -- run result (SimpleComponent : somestates) = do
    --     (logs, msgs) <- runStack setup (simple_handleEvent event)
    --     -- printLog logs
    --     run (SimpleComponent : result) somestates


-- data Component = forall s. Typeable s => Stateful
--                  { initState :: Z (StateT s IO) ()
--                  , someState :: s
--                  , stateHandler :: SomeEvent -> Z (StateT s IO) ()
--                  -- , receiveMessage :: SomeMessage -> Z m ()
--                  }
--                  | Stateless
--                  { eventHandler :: SomeEvent -> StatelessZ () }
--     deriving Typeable


data EventHandler b = forall a . (Event a) => EventHandler (a -> b)

data Config = Config
    { _modMask :: [ModMask]
    , _borderWidth :: Word
    , _normalBorderColor :: Word
    , _focusedBorderColor :: Word
    , _selectionBorderColor :: Word
    , _components :: [SomeComponent]
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

components :: Functor f => LensLike' f Config [SomeComponent]
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
type CursorMap   = Map Glyph CURSOR

data Setup = Setup
    { _config :: Config
    , _connection :: Connection
    , _rootWindow :: WindowId
    , _buttonMask :: [EventMask]
    , _keyboardMap :: KeyboardMap
    , _modifierMap :: ModifierMap
    , _glyphCursors :: CursorMap
    -- , _cursorMap :: CursorMap
    }
    deriving Typeable

config :: Functor f => LensLike' f Setup Config
config = lens _config (\d v -> d { _config = v })

connection :: Functor f => LensLike' f Setup Connection
connection = lens _connection (\d v -> d { _connection = v })

rootWindow :: Functor f => LensLike' f Setup WindowId
rootWindow = lens _rootWindow (\d v -> d { _rootWindow = v })

buttonMask :: Functor f => LensLike' f Setup [EventMask]
buttonMask = lens _buttonMask (\d v -> d { _buttonMask = v })

keyboardMap :: Functor f => LensLike' f Setup (Map KEYCODE [KEYSYM])
keyboardMap = lens _keyboardMap (\d v -> d { _keyboardMap = v })

modifierMap :: Functor f => LensLike' f Setup (Map MapIndex [KEYCODE])
modifierMap = lens _modifierMap (\d v -> d { _modifierMap = v })

glyphCursors :: Functor f => LensLike' f Setup (Map Glyph CURSOR)
glyphCursors = lens _glyphCursors (\d v -> d { _glyphCursors = v })


type LogWT = WriterT [String]

type MessageWT = WriterT [SomeMessage]

type SetupRT = ReaderT Setup

type Z m a = LogWT (MessageWT (SetupRT m)) a

type StatelessZ a = Z IO a
