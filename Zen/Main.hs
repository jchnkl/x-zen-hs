{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

import Control.Monad.State
import Control.Monad.Reader
import Graphics.XHB

import Core
import Types
import Config

-- TODO:
-- Lenses for data structures
-- Free Monads for Layout
-- Use Word32 for xids & convert to from WINDOW, DRAWABLE, etc.
-- Use motionNotifyEvent_root_x instead of root_x_MotionNotifyEvent

-- IDEAS
-- Use Mod4 with lock after timeout


main :: IO ()
main = connect >>= runLoop


runLoop :: Maybe Connection -> IO ()
runLoop Nothing = print "Got no connection!"
runLoop (Just c) = runReaderT (evalStateT runZ initialCore) initialSetup
    where
    initialCore = Core initialConfig initialWindowQueue (Position 0 0)
    initialSetup = ConnectionSetup c (getRoot c)
    initialWindowQueue = Queue []
