-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall #-}

module Log where

import Data.Time (getZonedTime)
import Control.Monad.Writer
import Types


toLog :: Monad m => String -> Z m ()
toLog s = tell [s]

appendLog :: Monad m => [String] -> Z m ()
appendLog s = tell s

printLog :: Log -> IO ()
printLog ls = when (not $ null ls) $ do
    time <- getZonedTime
    putStrLn . (show time ++) . ("\n" ++) . unlines . map ("\t" ++) $ ls
