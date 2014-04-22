{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Core where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Time (getZonedTime)
import Graphics.XHB hiding (Setup)

import Util
import Types hiding (Core(..))
import Event
-- import Config
-- import Setup
import qualified Client as Client

data Core = Core
    { _config :: Config
    , _queue :: Queue
    , _pointer :: Position
    }

config :: Lens Core Config
config = lens _config (\v d -> d { _config = v })

queue :: Lens Core Queue
queue = lens _queue (\v d -> d { _queue = v })

pointer :: Lens Core Position
pointer = lens _pointer (\v d -> d { _pointer = v })

type CoreST = StateT Core

run :: SomeEvent -> State Core ()
run event = undefined

runCore :: SomeEvent -> Core -> Core
runCore event = execState (run event)

-- runLog :: LogWT (CoreST (SetupRT IO)) () -> CoreST (SetupRT IO) [String]
-- runLog = execWriterT

-- initialCore :: Config -> Core
-- initialCore conf = Core conf (Queue []) (Position 0 0)

-- runCore :: CoreST (SetupRT IO) () -> Core -> SetupRT IO Core
-- runCore = execStateT


-- runCore :: Core -> CoreST (SetupRT IO) () -> IO ()
-- runCore core = do
--     c <- asksL connection
--     event <- waitForEvent c
--     local (\setup -> 

-- runLoop (Just c) = runReaderT (evalStateT runZ initialCore) initialSetup

{-

-- type ZCore = StateT Core (ReaderT Setup IO)
-- type Z = WriterT [String] ZCore

runZ :: ZCore ()
runZ = do
    c <- asksL connection

    let mask = CWEventMask
        values = toMask [EventMaskSubstructureRedirect, EventMaskSubstructureNotify]
        valueparam = toValueParam [(mask, values)]
    io $ changeWindowAttributes c (getRoot c) valueparam

    void $ execWriterT $ do
        C.grabKeys
        withRoot (io . queryTree c) >>= lift . io . getReply >>= manage

    -- Main loop
    forever $ do
        time <- fmap show $ io getZonedTime
        io . putStrLn . (show time ++) . ("\n" ++) . unlines . map ("\t" ++)
            =<< execWriterT (io (waitForEvent c) >>= dispatch)

    where
    manage (Left _) = return ()
    manage (Right tree) = do
        mapM_ C.grabButtons (children_QueryTreeReply tree)
        mapM_ C.insertWindow (children_QueryTreeReply tree)


manageMode :: Z ()
manageMode = undefined
-}
