{-# OPTIONS_GHC -Wall #-}

module Log where

import Control.Monad.Writer
import Types


toLog :: Monad m => String -> LogWT m ()
toLog s = tell [s]

