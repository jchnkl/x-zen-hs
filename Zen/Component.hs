-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Component where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Log
import Util
import Types


type ComponentRunner = forall m. MonadIO m
                     => Setup -> Model -> Component -> m ((Log, Model), Component)

type ComponentSink m = SomeSink (Z' m ())

data Component = forall d m. Monad m => Component
    { -- | Component data
      componentData    :: d
      -- | Pure evaluation function. Favor this!
    , pureRunComponent :: forall a. m a -> d -> (a, d)
      -- | Evaluation function with side effects
    , ioRunComponent   :: forall a. m a -> d -> IO (a, d)
    -- | Startup hook
    , onStartup        :: d -> Z' IO d
    -- | Shutdown hook
    , onShutdown       :: d -> Z' IO ()
    -- | List of event handlers
    , someSinks        :: d -> [ComponentSink m]
    }


execStack :: Monad m => Z' m () -> Setup -> Model -> m (Log, Model)
execStack f = runStateT . execWriterT . runReaderT f


runSinks :: (Monad m, Sink a)
         => a
         -> Setup
         -> Model
         -> [ComponentSink m]
         -> m (Log, Model)
runSinks event setup model sinks = execStack (run sinks) setup model
    where
    run []           = return ()
    run (sink:sinks') = do
        dispatch event sink
        run sinks'


execComponent :: (MonadIO m) => ComponentRunner -> Component -> Z' m (Log, Component)
execComponent f c = do
    setup <- ask
    model <- get
    ((runlog, model'), c') <- f setup model c
    put model'
    return (runlog, c')


pureExecComponent :: (MonadIO m, Sink e) => e -> Component -> Z' m (Log, Component)
pureExecComponent event = execComponent run
    where
    run setup model (Component d purerun iorun su sd csinks) = do
        let (a, d') = purerun (runSinks event setup model $ csinks d) d
        return (a, Component d' purerun iorun su sd csinks)


ioExecComponent :: (MonadIO m, Sink e) => e -> Component -> Z' m (Log, Component)
ioExecComponent event = execComponent run
    where
    run setup model (Component d purerun iorun su sd csinks) = do
        (a, d') <- io $ iorun (runSinks event setup model $ csinks d) d
        return (a, Component d' purerun iorun su sd csinks)
