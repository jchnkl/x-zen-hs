-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall           #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}

module Log where

import Control.Exception
import Data.Typeable
import Data.List as L (null)
import Data.Time (getZonedTime)
import Control.Monad.Writer
import System.IO
import System.Posix.Files
import Util (whenId)
import Types


data LogDestination = StdErr
                    | StdOut
                    | Fifo FilePath
                    | File FilePath
                    deriving (Show, Typeable)

toLog :: (MonadWriter Log m) => String -> m ()
toLog s = tell [s]

appendLog :: (MonadWriter Log m) => [String] -> m ()
appendLog s = tell s

ppComponentLog :: Component s -> [String] -> [String]
ppComponentLog c l = ppComponentId c : map ("\t"++) l ++ [ppComponentId c]

ppComponentId :: Component s -> String
ppComponentId c = "=== " ++ name c ++ " Component ==="
    where name (Component { componentId = cid }) = cid

appendComponentLog :: MonadWriter Log m => Component s -> [String] -> m ()
appendComponentLog c l = when (not $ L.null l) $ appendLog $ ppComponentLog c l

handlePrinter :: Handle -> Log -> IO ()
handlePrinter h ls = when (not $ null ls) $ do
    time <- getZonedTime
    hPutStrLn h . (show time ++) . ("\n" ++) . unlines . map ("\t" ++) $ ls

printer :: LogDestination -> Log -> IO ()
printer dest logs = case dest of
    StdErr  -> handlePrinter stderr logs
    StdOut  -> handlePrinter stdout logs
    File fp -> handle (printError . handleIOException)
               . withFile fp AppendMode $ flip handlePrinter logs
    Fifo fp -> do
        handle (printError . handleNamedPipeException fp) $
            fmap isNamedPipe (getFileStatus fp) >>= guard
        handle (printError . handleIOException) $
            withFile fp AppendMode (flip handlePrinter logs)

    where
    printError e = hPutStrLn stderr
                 $ "logPrinter failed" ++ whenId (not $ L.null e) (": " ++ e)
    handleIOException e
        | Just ioerror <- fromException e :: Maybe IOException =
            show ioerror ++ case dest of
                Fifo fp -> "\n(try `mkfifo '" ++ fp ++ "'` or `tail -f '" ++ fp ++ "'`)"
                _       -> ""
        | otherwise = "unknown error"
    handleNamedPipeException fp e
        | Just _ <- fromException e :: Maybe IOException = fp ++ " is not a named pipe"
        | otherwise                                      = "unknown error"
