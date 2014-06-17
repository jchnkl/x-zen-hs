{-# OPTIONS_GHC -Wall #-}

module Log where

import Data.Time (getZonedTime)
import Control.Monad.Writer
import Types


type Log = [String]


toLog :: Monad m => String -> Z m ()
toLog s = tell [s]


printLog :: Log -> IO ()
printLog ls = when (not $ null ls) $ do
    time <- getZonedTime
    putStrLn . (show time ++) . ("\n" ++) . unlines . map ("\t" ++) $ ls
