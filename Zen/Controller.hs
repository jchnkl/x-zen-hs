-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification, RecordWildCards, RankNTypes #-}

module Controller where

-- import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Exception (bracket)
-- import Control.Concurrent
-- import Control.Concurrent.STM
-- import Graphics.XHB (Event, SomeEvent, fromEvent)

import Log
import Util
-- import Lens
import Types


type ComponentSink m = SomeSink (Z' m ())
-- type ComponentSink = SomeSink (Z'' ())

data Component = forall d m. Monad m => Component
    { -- | Component data
      componentData :: d
      -- | Pure evaluation function. Favor this!
    , pureComponent :: forall a. m a -> d -> (a, d)
      -- | Evaluation function with side effects
    , runComponent  :: forall a. m a -> d -> IO (a, d)
    -- | Startup hook
    , onStartup     :: d -> Z' IO d
    -- | Shutdown hook
    , onShutdown    :: d -> Z' IO ()
    -- | List of event handlers
    , someSinks     :: d -> [ComponentSink m]
    }

-- setStartup :: (forall d. d -> Z' IO d) -> Component -> Component
-- setStartup f (Component {..}) = Component { onStartup = f, ..}

-- setShutdown :: (forall d. d -> Z' IO ()) -> Component -> Component
-- setShutdown f (Component {..}) = Component { onShutdown = f, ..}

-- setSinks :: (forall d m. Monad m => d -> [ComponentSink m])
--          -> Component
--          -> Component
-- setSinks f (Component {..}) = Component { someSinks = f, ..}

-- setPure :: (forall a m d. m a -> d -> (a, d))
--         -> Component
--         -> Component
-- setPure f (Component {..}) = Component { pureComponent = f, ..}

-- setRun :: (forall a m d. Monad m => m a -> d -> IO (a, d))
--        -> Component
--        -> Component
-- setRun f (Component {..}) = Component { runComponent = f, ..}

-- mkComponent :: (Monad m, Functor m)
--             => d
--             -> (forall a. m a -> d -> (a, d))
--             -> Component
-- mkComponent d f = Component
--     d
--     f
--     (\m d -> return $ f m d)
--     (return . id)
--     (const $ return ())
--     (const [])


-- foo = mkComponent Foo runState

-- withComponentData :: (forall d. d -> a) -> Component -> a
-- withComponentData f (Component d _ _ _ _) = f d

-- modifyComponentData :: (forall d. d -> d) -> Component -> Component
-- modifyComponentData f (Component d r su sd s) = Component (f d) r su sd s

-- withComponentSinks :: (forall m. [ComponentSink m] -> a) -> Component -> a
-- withComponentSinks f (Component d _ _ _ sinks) = f (sinks d)




execComponent :: (MonadIO m, Sink a) => a -> Component -> Z' m ([String], Component)
execComponent event (Component cdata purec runc su sd csinks) = do
    setup <- ask
    model <- get
    ((runlog', model'), cdata') <- io $ runc (run setup model [] $ csinks cdata) cdata
    put model'
    return (runlog', Component cdata' purec runc su sd csinks)

    where
    run setup model runlog []           = return (runlog, model)
    run setup model runlog (sink:sinks) = do
        (runlog', model') <- execStack (dispatch event sink) setup model
        run setup model (runlog ++ runlog') sinks



execStack :: Monad m => Z' m () -> Setup -> Model -> m ([String], Model)
execStack f = runStateT . execWriterT . runReaderT f


runStack :: Z' m b -> Setup -> Model -> m ((b, [String]), Model)
runStack f = runStateT . runWriterT . runReaderT f


-- execStack' :: Monad m => Z'' () -> Setup -> Model -> m ([String], Model)
-- execStack' f = runStateT . execWriterT . runReaderT f

execPureComponent :: (Monad m, Sink a) => a -> Component -> Z' m ([String], Component)
execPureComponent event (Component cdata purec runc su sd csinks) = do
    setup <- ask
    model <- get
    let ((runlog', model'), cdata') = purec (run setup model [] (csinks cdata)) cdata
    put model'
    return (runlog', Component cdata' purec runc su sd csinks)

    where
    run _     model runlog []           = return (runlog, model)
    run setup model runlog (sink:sinks) = do
        (runlog', model') <- execStack (dispatch event sink) setup model
        run setup model' (runlog ++ runlog') sinks


{-
getConfig :: Typeable a => [ComponentConfig] -> Maybe a
getConfig (ComponentConfig c:cs) = case cast c of
    Just c' -> Just c'
    Nothing -> getConfig cs
getConfig _ = Nothing


execComponent :: Sink a => Setup -> a -> Component -> IO Component
execComponent setup event (Component cdata runc su sd hs) =
    run >>= _1 (printLog . snd) >>= returnComponent . snd
    where
    run = flip runc cdata $ runStack (mapM (dispatch event) (hs cdata)) setup
    returnComponent d = return $ Component d runc su sd hs


withComponents :: Setup -> ([TMVar Component] -> IO a) -> IO a
withComponents setup = bracket startup shutdown
    where
    startup = do
        cvars <- mapM newTMVarIO $ setup ^. config . components
        mapM_ (forkIO . startupComponent setup) cvars
        return cvars

    shutdown = mapM_ (shutdownComponent setup)


startupComponent :: Setup -> TMVar Component -> IO ()
startupComponent setup = flip modifyTMVarM exec
    where
    exec (Component cdata runc startupc sd hs) = do
        runStack (startupc cdata) setup
            >>= _2 printLog
                >>= return . \(d, _) -> Component d runc startupc sd hs


shutdownComponent :: Setup -> TMVar Component -> IO ()
shutdownComponent setup = flip withTMVarM exec
    where
    exec (Component cdata _ _ shutdownc _) =
        runStack (shutdownc cdata) setup >>= printLog . snd
-}
