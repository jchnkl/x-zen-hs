-- vim: set sw=4 sws=4 ts=4

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Model where


-- import Data.Maybe (catMaybes, fromMaybe)
-- import Data.Typeable
-- import Data.Map (Map)
-- import qualified Data.Map as M
-- import Data.List ((\\))
-- import qualified Data.List as L
import Control.Monad.State
-- import Control.Applicative
-- import Graphics.XHB
import Types
-- import Util
import Lens
-- import Log
-- import Message
import qualified Queue as Q
import qualified Window as W
-- import qualified Keyboard as K
-- import Graphics.X11.Types (KeySym, xK_Num_Lock, xK_Caps_Lock)

{-
 TODO
 * Follow focus:
     - border color
     - warp mouse pointer when focus change by key
-}


insertClient :: Monad m => Client -> Z m ()
insertClient = modify . (model %~) . Q.insert


insert :: Monad m => WindowId -> Z m ()
insert window = modify $ model %~ Q.insert (Client window nullPosition nullGeometry)


remove :: Monad m => WindowId -> Z m ()
remove = modify . (model %~) . Q.remove


raise :: MonadIO m => WindowId -> Z m ()
raise = W.raise


lower :: MonadIO m => WindowId -> Z m ()
lower = W.lower


member :: Monad m => WindowId -> Z m Bool
-- member w = gets (^. (model . to (Q.member w)))
member w = gets (^. model . to (Q.member w))
-- \w -> gets ( ^. (model . to (member w)))
-- :t m ^. model . (to $ member (undefined :: WindowId ))


lookup :: Monad m => WindowId -> Z m (Maybe Client)
lookup w = gets (^. model . to (Q.lookup w))


asList :: Monad m => Z m [Client]
asList = gets (^. model . to Q.toList)


modifyClientM :: Monad m => WindowId -> (Client -> Client) -> Z m ()
modifyClientM w = modify . (model %~) . Q.modifyClient w


withQueueM :: (Functor m, MonadIO m) => (Queue -> Z m a) -> Z m a
withQueueM f = getL model >>= f



-- runModel :: Monad m => ModelT m a -> ClientStack -> m (a, ClientStack)
-- runModel = runStateT

above :: Functor f => LensLike' f ClientStack [Client]
above = lens _above (\d v -> d { _above = v })

focus :: Functor f => LensLike' f ClientStack Client
focus = lens _focus (\d v -> d { _focus = v })

below :: Functor f => LensLike' f ClientStack [Client]
below = lens _below (\d v -> d { _below = v })

focusNext :: ClientStack -> ClientStack
focusNext (ClientStack [] f []) = ClientStack []          f         []
focusNext (ClientStack as f []) = ClientStack []          (head as) (tail as ++ [f])
focusNext (ClientStack as f bs) = ClientStack (as ++ [f]) (head bs) (tail bs)

focusPrev :: ClientStack -> ClientStack
focusPrev (ClientStack [] f []) = ClientStack []            f         []
focusPrev (ClientStack [] f bs) = ClientStack (f : init bs) (last bs) []
focusPrev (ClientStack as f bs) = ClientStack (as ++ [f])   (head bs) (tail bs)


-- data Model = Model { _clients :: ClientStack }
--     deriving Typeable
--
-- clients :: Functor f => LensLike' f Model ClientStack
-- clients = lens _clients (\d v -> d { _clients = v })

integrate :: ClientStack -> [Client]
integrate (ClientStack as fc bs) = as ++ fc : bs


withClient :: (Client -> a) -> WindowId -> ClientStack -> Maybe a
withClient f w = exec . integrate
    where exec []                     = Nothing
          exec (c:cs) | c ^. xid == w = Just $ f c
                      | otherwise     = exec cs


modifyClient :: (Client -> Client) -> WindowId -> ClientStack -> ClientStack
modifyClient f w (ClientStack as fc bs)
    | w == fc ^. xid = ClientStack as (f fc) bs
    | otherwise      = ClientStack (mapf as) fc (mapf bs)
    where mapf []                     = []
          mapf (c:cs) | c ^. xid == w = f c : cs
                      | otherwise     = c : mapf cs
