-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Log where

import Control.Exception
import Data.List as L (null)
import Data.Time (getZonedTime)
import Control.Monad.Writer
import System.IO
import System.Posix.Files
import Types


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

stdoutPrinter :: Log -> IO ()
stdoutPrinter = handlePrinter stdout

stderrPrinter :: Log -> IO ()
stderrPrinter = handlePrinter stderr

fifoPrinter :: FilePath -> Log -> IO ()
fifoPrinter fp ls = handle (printError . handleException) $
    fmap isNamedPipe (getFileStatus fp) >>= \case
        True -> withFile fp AppendMode (flip handlePrinter ls)
        _    -> printError isNotNamedPipe
    where
    printError = hPutStrLn stderr
    isNotNamedPipe  = "fifoPrinter failed, " ++ fp ++ " is not a named pipe"
    handleException e
        | Just _ <- fromException e :: Maybe IOException =
            "fifoPrinter failed, try `mkfifo '" ++ fp ++ "'` or `tail -f '" ++ fp ++ "'`"
        | otherwise = "fifoPrinter failed, unknown error"
