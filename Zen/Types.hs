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
             -- DatatypeContexts,

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

printLog :: [String] -> IO ()
printLog = print . show

data ComponentConfig = forall a. (Show a, Typeable a) => ComponentConfig a
    deriving Typeable

deriving instance Show ComponentConfig

data SomeMessage = forall a. (Typeable a) => SomeMessage a
    deriving Typeable


sendMessage :: Typeable s => SomeMessage -> Z (StateT s IO) ()
sendMessage _ = do
    _ <- askL (config . components)
    -- forM_ cs $ send msg
    -- res <- send [] cs
    -- putL (config . components) res
    return ()
    -- where


class Typeable a => Component a where
    fromComponent :: SomeComponent2 -> Maybe a
    -- toComponent :: a -> SomeComponent


class Typeable a => ComponentRunner a where
    run :: Setup -> SomeEvent -> a -> IO a

instance ComponentRunner PureComponent2 where
    run setup event (PureComponent2 hevent) = do
        (logs, _) <- runStack setup (hevent event)
        printLog logs
        return $ PureComponent2 hevent

instance Typeable s => ComponentRunner (StateComponent2 s) where
    run setup event (StateComponent2 s hevent hmsg) = do
        ((logs, _), s') <- runStateT (runStack setup (hevent event)) s
        printLog logs
        return $ StateComponent2 s' hevent hmsg

instance Component PureComponent2 where
    fromComponent (SomeComponent2 c) = cast c

instance Typeable s => Component (StateComponent2 s) where
    fromComponent (SomeComponent2 c) = cast c

data SomeComponent2 = forall a. (ComponentRunner a) => SomeComponent2 a

data PureComponent2 = PureComponent2 (SomeEvent -> Z IO ())
    deriving Typeable

data StateComponent2 s = StateComponent2
    s
    (SomeEvent -> Z (StateT s IO) ())
    (SomeMessage -> Z (StateT s IO) ())
    deriving Typeable


whenJust :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
whenJust Nothing _ = return Nothing
whenJust (Just v) f = liftM Just (f v)

runComponents2 :: Setup -> SomeEvent -> [SomeComponent2] -> IO [SomeComponent2]
runComponents2 setup event = run' []
    where
    run' result (sc:scs) = do
        _ <- whenJust (fromComponent sc :: Maybe PureComponent2) $ run setup event
        run' result scs
    run' result _ = return result





{-
data SomeComponent3 = forall c. (c -> IO c) =>
    -- SomeComponent3 c (Setup -> SomeEvent -> c -> IO c)
    SomeComponent3 c

foo :: Int -> IO Int
foo = undefined

myStateComponent3 = SomeComponent3 foo

runComponents3 :: Setup -> SomeEvent -> [SomeComponent3] -> IO [SomeComponent3]
runComponents3 setup event = run' []
    where
    run' result (_ : scs) = do
    -- run' result (SomeComponent3 c f : scs) = do
    --     c' <- f setup event c
        run' result scs
    run' result _ = return result


data SomeComponent4 = forall a c. (Typeable c) => SomeComponent4
    c
    (Setup -> SomeEvent -> c -> IO (([String], [SomeMessage]), c))


myStateComponentHandleEvent4 :: SomeEvent -> Z (StateT Int IO) ()
myStateComponentHandleEvent4 = undefined

myStateComponentFun4 :: Setup -> SomeEvent -> Int -> IO (([String], [SomeMessage]), Int)
myStateComponentFun4 setup event s =
    runStateT (runStack setup (myStateComponentHandleEvent4 event)) s

myStateComponent4 = SomeComponent4 (1::Int) myStateComponentFun4
-}

data MyStateComponent5 = MyStateComponent5
    Int
    (SomeEvent -> Z (StateT Int IO) ())
    deriving Typeable

runMyStateComponent5 :: (Z (StateT Int IO) () -> (StateT Int IO) a)
                     -> SomeEvent
                     -> MyStateComponent5
                     -> IO (a, MyStateComponent5)
runMyStateComponent5 f event (MyStateComponent5 s ehandler) = do
    (a, s') <- runStateT (f (ehandler event)) s
    return (a, MyStateComponent5 s' ehandler)

data SomeComponent5 = forall m c. (MonadIO m, Typeable c) => SomeComponent5
    { component :: c
    , runComponent :: (Z m () -> m ([String], [SomeMessage]))
                   -> SomeEvent
                   -> c
                   -> IO (([String], [SomeMessage]), c)
    }

mySomeComponent5 :: SomeComponent5
mySomeComponent5 = SomeComponent5
    (MyStateComponent5 1 (\_ -> return ()))
    runMyStateComponent5



runComponents5 :: Setup -> SomeEvent -> [SomeComponent5] -> IO [SomeComponent5]
runComponents5 setup event = run' []
    where
    run' result (SomeComponent5 c f : scs) = do
        _ <- f (runStack setup) event c
        run' result scs
    run' result _ = return result





runStack :: MonadIO m => Setup -> Z m () -> m ([String], [SomeMessage])
runStack setup f = runReaderT (runWriterT (execWriterT f)) setup


{-
data SomeComponent

    = PureComponent
        (SomeEvent -> Z IO ())

    | forall s. (Typeable s) => StateComponent
        s
        (SomeEvent -> Z (StateT s IO) ())
        (SomeMessage -> Z (StateT s IO) ())



myPureComponent :: SomeComponent
myPureComponent = PureComponent
    (\_ -> (tell ["tell myPureComponent"]) >> (liftIO . print $ "myPureComponent"))

myStateComponent :: SomeComponent
myStateComponent = StateComponent
    (1::Int)
    (\_ -> (tell ["tell myStateComponent"]) >> (get >>= liftIO . print . ("myStateComponent with state: " ++) . show))
    (\_ -> (tell ["tell myStateComponent"]) >> (get >>= liftIO . print . ("myStateComponent with state: " ++) . show))

myComponents :: [SomeComponent]
myComponents = [myPureComponent, myStateComponent]

runStack :: MonadIO m => Setup -> Z m () -> m ([String], [SomeMessage])
runStack :: MonadIO m => Setup -> Z m () -> m ([String], [SomeMessage])
runStack setup f = runReaderT (runWriterT (execWriterT f)) setup

runStack setup f = runReaderT (runWriterT (execWriterT f)) setup

runComponents :: Setup -> SomeEvent -> [SomeComponent] -> IO [SomeComponent]
runComponents setup event = run []
    where

    run result (StateComponent s hevent hmsg : rest) = do
        ((logs, msgs), s') <- runStateT (runStack setup (hevent event)) s
        printLog logs
        run (StateComponent s' hevent hmsg : result) rest

    run result (PureComponent hevent : rest) = do
        (logs, msgs) <- runStack setup (hevent event)
        printLog logs
        run (PureComponent hevent : result) rest

    run result _ = return result
-}


data EventHandler b = forall a . (Event a) => EventHandler (a -> b)

data Config = Config
    { _modMask :: [ModMask]
    , _borderWidth :: Word
    , _normalBorderColor :: Word
    , _focusedBorderColor :: Word
    , _selectionBorderColor :: Word
    , _components :: [Int]
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

components :: Functor f => LensLike' f Config [Int]
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
