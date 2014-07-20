-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE FlexibleContexts #-}

module Log where

import Data.Time (getZonedTime)
import Control.Monad.Writer
import Types


toLog :: (MonadWriter Log m) => String -> m ()
toLog s = tell [s]

appendLog :: (MonadWriter Log m) => [String] -> m ()
appendLog s = tell s

printLog :: Log -> IO ()
printLog ls = when (not $ null ls) $ do
    time <- getZonedTime
    putStrLn . (show time ++) . ("\n" ++) . unlines . map ("\t" ++) $ ls
