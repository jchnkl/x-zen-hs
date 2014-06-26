-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Component where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Catch (bracket)

import Log
import Lens
import Util
import Types


withComponents :: ([Component] -> ModelST (SetupRT IO) a) -> ModelST (SetupRT IO) a
withComponents = bracket startup shutdown
    where
    startup = askL (config . components) >>= mapM startupComponent
    shutdown = mapM_ shutdownComponent


startupComponent :: Component -> ModelST (SetupRT IO) Component
startupComponent (Component cid d r startup su h) = do
    (d', l) <- runWriterT (startup d)
    io . printLog $ ("startup " ++ cid ++ ":") : l
    return $ Component cid d' r startup su h


shutdownComponent :: Component -> ModelST (SetupRT IO) ()
shutdownComponent (Component cid d _ _ shutdown _) = do
    l <- execWriterT (shutdown d)
    io . printLog $ ("shutdown " ++ cid ++ ":") : l


-- type ComponentRunner = forall m. MonadIO m
--                      => Setup -> Model -> Component -> m ((Log, Model), Component)

-- type ComponentSink m = SomeSink (Z' m ())
-- type ComponentSink' m = SomeSink' (Z' m ())



-- instance Handler SomeEvent where
--     type CastClass SomeEvent = Event
--     dispatch' se c f = case (fromEvent se) of
--         Just e -> f e c
--         _ -> return c

-- instance Handler SomeMessage where
--     type CastClass SomeMessage = Message
--     dispatch' me c f = case (fromMessage me) of
--         Just e -> f e c
--         _ -> return c

-- instance Handler SomeMessage

-- data SomeHandler = Finish
--                  | forall a. Handler a => SomeHandler SomeHandler a


-- barz :: SomeHandler
-- barz = SomeHandler (

runStack :: Monad m => Setup -> Model -> Z m a -> m ((a, Log), Model)
runStack setup model f = runReaderT (runStateT (runWriterT f) model) setup


execStack :: Monad m => Setup -> Model -> Z m () -> m (Log, Model)
execStack setup model f = runReaderT (runStateT (execWriterT f) model) setup


{-
runSinks' :: (Monad m, Functor m, Item a)
         => a
         -> Setup
         -> Model
         -> [ComponentSink' m]
         -> m (Log, Model)
runSinks' event setup model sinks = execStack (run sinks) setup model
    where
    run []           = return ()
    run (SomeSink' f:sinks') = do
        f event
        -- dispatch event sink
        run sinks'


pureExecComponent' :: (MonadIO m, Item e) => e -> Component -> Z' m (Log, Component)
pureExecComponent' event = execComponent run
    where
    run setup model (Component d purerun iorun su sd csinks) = do
        let (a, d') = purerun (runSinks' event setup model $ csinks d) d
        return (a, Component d' purerun iorun su sd csinks)
        -}


-- runSinks :: (Monad m, Functor m, Sink a)
--          => a
--          -> Setup
--          -> Model
--          -> [ComponentSink m]
--          -> m (Log, Model)
-- runSinks event setup model sinks = execStack (run sinks) setup model
--     where
--     run []           = return ()
--     run (sink:sinks') = do
--         dispatch event sink
--         run sinks'


-- execComponent :: (MonadIO m) => ComponentRunner -> Component -> Z' m (Log, Component)
-- execComponent f c = do
--     setup <- ask
--     model <- get
--     ((runlog, model'), c') <- f setup model c
--     put model'
--     return (runlog, c')


-- pureExecComponent :: (MonadIO m, Sink e) => e -> Component -> Z' m (Log, Component)
-- pureExecComponent event = execComponent run
--     where
--     run setup model (Component d purerun iorun su sd csinks) = do
--         let (a, d') = purerun (runSinks event setup model $ csinks d) d
--         return (a, Component d' purerun iorun su sd csinks)


-- ioExecComponent :: (MonadIO m, Sink e) => e -> Component -> Z' m (Log, Component)
-- ioExecComponent event = execComponent run
--     where
--     run setup model (Component d purerun iorun su sd csinks) = do
--         (a, d') <- io $ iorun (runSinks event setup model $ csinks d) d
--         return (a, Component d' purerun iorun su sd csinks)
