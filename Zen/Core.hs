{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Core where

import Control.Monad.State
import Control.Monad.Writer
import Data.Time (getZonedTime)
import Graphics.XHB

import Util
import Types
import Event
import qualified Client as C

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
