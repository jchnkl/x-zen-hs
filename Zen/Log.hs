{-# OPTIONS_GHC -Wall #-}

module Log where

import Data.Time (getZonedTime)
import Control.Monad.Writer
import Types


toLog :: Monad m => String -> Z m ()
toLog s = tell [s]


printLog :: [String] -> IO ()
printLog ls = do
    time <- getZonedTime
    putStrLn . (show time ++) . ("\n" ++) . unlines . map ("\t" ++) $ ls
