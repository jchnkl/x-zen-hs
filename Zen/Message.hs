-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable, GADTs #-}
-- {-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, StandaloneDeriving
--    #-}

module Message
    ( SomeReply(..)
    , SomeMessage(..)
    , MessageCom
    , sendMessage
    , sendReply
    -- , waitForMessageReply
    -- , tryFetchReply
    , messageSource
    ) where

-- import GHC.Exts (Constraint)
-- import GHC.Generics (Generic)
-- import Data.Dynamic
import Data.Maybe (fromMaybe)
import Data.Typeable
-- import Data.Hashable
-- import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.STM
import System.IO.Unsafe (unsafePerformIO)
-- import qualified Graphics.XHB as X

import Util
import Types


messageChan :: TMVar MessageCom
messageChan = unsafePerformIO newEmptyTMVarIO


messageSource :: IO MessageCom
messageSource = atomically $ takeTMVar messageChan


sendMessage :: (MonadIO m, Message a, Reply b) => a -> Z m (Maybe b)
sendMessage v = io $ do
    reply <- newEmptyTMVarIO
    atomically . putTMVar messageChan $ MessageCom reply (toMessage v)
    fromReply <$> atomically (takeTMVar reply)


sendReply :: (MonadIO m, Message a, Reply b) => (a -> Z m b) -> MessageCom -> Z m ()
sendReply f (MessageCom rc sm) =
    whenJustM (fromMessage sm) f >>= io . atomically . putTMVar rc . toReply
