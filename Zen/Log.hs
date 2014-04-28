{-# OPTIONS_GHC -Wall #-}

module Log where

import Control.Monad.Writer
import Types


toLog :: String -> Z ()
toLog s = tell [s]
