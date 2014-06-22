-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ImpredicativeTypes, FlexibleInstances, NoMonomorphismRestriction, DeriveDataTypeable, ConstraintKinds, TypeFamilies, ExistentialQuantification, GADTs, RankNTypes, StandaloneDeriving, ScopedTypeVariables #-}

module Controller where

import GHC.Exts
import Data.Typeable
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Concurrent
import Control.Concurrent.STM
import Graphics.XHB hiding (Setup) -- (Event, SomeEvent, fromEvent, waitForEvent)

import Log
import Lens
import Util
import Types hiding (Sink, dispatch)
import Component


data SomeEventHandler = forall a. (Event a) =>
    SomeEventHandler (forall m. MonadIO m => a -> Z' m ())
    deriving (Typeable)

instance Handler SomeEventHandler

class Dispatcher a where
    dispatch :: MonadIO m => a -> SomeHandler -> Z' m ()

instance Dispatcher SomeEvent where
    dispatch se sh = case fromHandler sh :: Maybe SomeEventHandler of
        Just (SomeEventHandler f) -> case fromEvent se of
            Just e -> f e
            _ -> return ()
        _ -> return ()

data AnyEvent = forall a. Dispatcher a => AnyEvent a

type EventSource = Setup -> IO AnyEvent


xEventSource :: Setup -> IO AnyEvent
xEventSource setup = AnyEvent <$> waitForEvent (setup ^. connection)


execAnyEvent :: Setup -> Model -> AnyEvent -> Component -> IO ((Log, Model), Component)
execAnyEvent setup model (AnyEvent e) (Component d runpure runio su sd somehandlers) = do
    ((runlog, model'), d') <- exec [] model d $ somehandlers d
    return $ ((runlog, model'), Component d' runpure runio su sd somehandlers)
    where
    exec runlog model cdata []     = return ((runlog, model), cdata)
    exec runlog model cdata (h:hs) = do
        ((runlog', model'), cdata') <- run model cdata h
        exec (runlog ++ runlog') model' cdata' hs
    run model cdata h = runio (execStack (dispatch e h) setup model) cdata


dispatchAnyEvent :: AnyEvent -> Component -> Z' IO Component
dispatchAnyEvent (AnyEvent e) (Component d runpure runio su sd handlers) = do
    (runlog, d') <- exec [] d $ handlers d
    -- toLog $ showLog runlog
    return $ Component d' runpure runio su sd handlers
    where
    exec runlog cdata []     = return (runlog, cdata)
    exec runlog cdata (h:hs) = do
        (runlog', cdata') <- run cdata h
        -- toLog runlog'
        exec (runlog ++ runlog') cdata' hs

    run cdata h = do
        s <- ask
        m <- get
        ((rlog, m'), cdata') <- io $ runio (execStack (dispatch e h) s m) cdata
        put m'
        return (rlog, cdata')


deriving instance Typeable SomeEvent

-- instance Item SomeEvent

type ItemSource = TChan SomeItem
type Producer = Setup -> ItemSource -> IO ()
type Consumer = SomeItem -> Component -> Z' IO (Log, Component)


-- foo :: [SomeSink' (Z' IO ())]
-- foo = [SomeSink' eventConsumer]

-- eventConsumer :: Item a => a -> Z' IO ()
-- eventConsumer = undefined


-- xEventProducer :: Setup -> IO SomeItem
xEventProducer :: Setup -> ItemSource -> IO ()
xEventProducer setup chan = forever
                          $ waitForEvent (setup ^. connection)
                            >>= atomically . writeTChan chan . SomeItem


-- xEventConsumer :: MonadIO m => SomeItem -> Component -> Z' m (Log, Component)
-- xEventConsumer i component
--     | Just (e :: SomeEvent) <- fromItem i = pureExecComponent e component
--     | otherwise                           = return ([], component)

-- pureExecComponent :: (MonadIO m, Sink e) => e -> Component -> Z' m (Log, Component)

producer :: [Producer]
producer = [xEventProducer]

type Producer' = Setup -> IO SomeItem

data SomeItem = forall a. Caster a => SomeItem a

-- class Typeable a => Item a where
--     fromItem :: SomeItem -> Maybe a
--     fromItem (SomeItem i) = cast i


-- consumer :: [Consumer]
-- consumer = [xEventConsumer]

class Typeable a => Caster a where
    type CastConstraint a :: * -> Constraint
    -- type CastConstraint a = Typeable
    docast :: CastConstraint a b => a -> Maybe b
    -- docast = cast

instance Caster SomeEvent where
    type CastConstraint SomeEvent = Event
    docast = fromEvent

instance Caster SomeMessage where
    type CastConstraint SomeMessage = Message
    docast = fromMessage

data Component'' = Component''
    { updateComponent :: forall a. (a -> Component'' -> Z' IO Component'') }

-- myComponent :: Component''
-- myComponent = Component'' (bar')

foo :: (Caster a, CastConstraint a b) => a -> (b -> IO ()) -> IO ()
foo a f = case (docast a) of
    Just b -> print "yay" >> f b
    _ -> print "nay" >> return ()


-- morefoo :: Producer -> Component'' -> Z' IO Component''

data FooMessage = FooMessage deriving Typeable

instance Message FooMessage

bar' :: FooMessage -> Component'' -> Z' IO Component''
bar' = undefined

bar :: FooMessage -> IO ()
bar _ = print "FooMessage"

baz :: KeyPressEvent -> IO ()
baz _ = print "KeyPressEvent"

-- ||   foobaz :: forall a. Typeable a => a -> IO ()
foobaz :: SomeEvent -> IO ()
foobaz se = foo se baz

foobar :: SomeMessage -> IO ()
foobar me = foo me bar

-- -- doit :: [(forall a. a => a -> Component' -> Z' IO Component) -> SomeItem -> Component' -> Z' IO Component']
-- doit :: (forall a. Typeable a => a -> Component' -> Z' IO Component')
--      -> SomeItem
--      -> Component'
--      -> Z' IO Component'
-- doit f (SomeItem i) component@(Component' uf) = do
--     f (cast i) component

--     -- dispatch' :: CastClass a b => a -> Component' -> (b -> Component' -> Z' IO Component') -> Z' IO Component'
-- dispatch'' :: 
--            Setup
--            -> Component'
--            -> (Setup -> Z' IO SomeItem)
--            -> Z' IO ()
-- dispatch'' setup component@(Component' uf) f = do
--     (SomeItem i) <- f setup
--     dispatch' i component uf

    -- return ()


runProducer :: Setup -> [Producer] -> IO [(ThreadId, ItemSource)]
runProducer setup = mapM $ \f -> do
    chan <- newTChanIO
    tid <- forkIO $ f setup chan
    return (tid, chan)


readItem :: [ItemSource] -> IO SomeItem
readItem = atomically . foldr1 orElse . map readTChan


runConsumer :: SomeItem -> [Consumer] -> [Component] -> Z' IO [(Log, Component)]
runConsumer item consumer = fmap concat . forM consumer . run
    where run components consume = mapM (consume item) components


main setup model consumer components = do
    tids_chans <- runProducer setup producer

    -- execStack :: Monad m => Z' m () -> Setup -> Model -> m (Log, Model)
    readItem (map snd tids_chans) >>= \i -> execStack (run i) setup model
    return ()
    where run i = void $ runConsumer i consumer components



main' :: Setup -> Model -> Component -> IO ()
main' setup model component = do
    ae <- xEventSource setup
    -- ((log, model'), component') <- execStack (dispatchAnyEvent ae component) setup model

-- execStack :: Monad m => Z' m () -> Setup -> Model -> m (Log, Model)
    -- printLog log
    -- shf <- xEventSource setup
    -- let foo = exec component shf
    -- xEventSource setup bro00 fro00 component
    -- runStateT (runWriterT (runReaderT (runComponent' shf component) setup)) model
    return ()

    where
    somehandler = undefined :: SomeHandler


runController :: Setup -> [Setup -> IO AnyEvent] -> IO [(ThreadId, TChan AnyEvent)]
runController setup = mapM run
    where
    run f = do
        chan <- newTChanIO
        tid <- forkIO $ forever $ f setup >>= atomically . writeTChan chan
        return (tid, chan)


runConsumers :: [TChan AnyEvent] -> [Component] -> Z' IO ()
runConsumers chans components = readAnyEvent >>= run components
    where
    run cs ae = mapM (dispatchAnyEvent ae) cs >>= (readAnyEvent >>=) . run
    readAnyEvent = io . atomically . foldr1 orElse . map readTChan $ chans
