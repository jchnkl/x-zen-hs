{-# OPTIONS_GHC -Wall #-}

module Log where

import Control.Monad.Writer

type LogWT = WriterT [String]

showLog :: [String] -> String
showLog = ("\n" ++) . unlines . map ("\t" ++)

runLog :: Monad m => LogWT m () -> m [String]
runLog = execWriterT
