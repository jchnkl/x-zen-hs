-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE FlexibleContexts #-}

module Log where

import Data.List as L (null)
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

ppComponentLog :: Component s -> [String] -> [String]
ppComponentLog c l = ppComponentId c : map ("\t"++) l ++ [ppComponentId c]

ppComponentId :: Component s -> String
ppComponentId c = "=== " ++ name c ++ " Component ==="
    where name (Component { componentId = cid }) = cid

appendComponentLog :: MonadWriter Log m => Component s -> [String] -> m ()
appendComponentLog c l = when (not $ L.null l) $ appendLog $ ppComponentLog c l
