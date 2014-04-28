module Queue where

import Prelude hiding (map)
import qualified Data.Map as M
import qualified Data.List as L
import Util
import Types -- hiding (queue) -- , focus)
-- import qualified Types as T (focus)

import LensUtil
-- import Lens.Family
-- import Lens.Family.State

insert :: Client -> Queue -> Queue
insert client = M.insert (client ^. xid) client

remove :: WindowId -> Queue -> Queue
remove = M.delete

withClient :: WindowId -> (Client -> a) -> Queue -> Maybe a
withClient w f = fmap f . M.lookup w

-- TODO: more generic combinator?
withClient' :: WindowId -> (Client -> Z a) -> Z (Maybe a)
withClient' w f = queue $*> flip whenJust f . M.lookup w 

-- Queue.hs|28 col 19 error| Couldn't match type `Setup' with `Core'
-- || When using functional dependencies to combine
-- ||   MonadReader r (ReaderT r m),
-- ||     arising from the dependency `m -> r'
-- ||     in the instance declaration in `Control.Monad.Reader.Class'
-- ||   MonadReader Core (ReaderT Setup IO),
-- ||     arising from a use of `Queue.take'
-- ||     at /home/jrk/prj/sandbox/haskell_ffi/zen/Zen/Queue.hs:28:19-28
-- || In the first argument of `(>>=)', namely `Queue.take queue'
-- || In the expression:
-- ||   Queue.take queue >>= flip whenJust f . M.lookup w

modifyClient :: WindowId -> (Client -> Client) -> Queue -> Queue
modifyClient w f = M.alter (fmap f) w

-- flatten :: Queue -> [Client]
-- flatten (Queue a Nothing b) = a ++ b
-- flatten (Queue a (Just c) b) = c : a ++ b


-- insert :: Client -> Queue -> Queue
-- insert client queue@(Queue _ Nothing _) = (focus ^= Just client) queue
-- insert client (Queue a (Just c) b) = Queue a (Just client) (c : b)


-- remove :: (Client -> Bool) -> Queue -> Queue
-- remove f (Queue a Nothing b) = Queue (filter f a) Nothing (filter f b)
-- remove f (Queue a (Just c) b)
--     | f c = Queue a Nothing b
--     | otherwise = Queue (filter f a) (Just c) (filter f b)


-- map :: (Client -> Client) -> Queue -> Queue
-- map f (Queue a c b) = Queue (L.map f a) (fmap f c) (L.map f b)


-- with :: (Client -> Bool) -> (Client -> Client) -> Queue -> Queue
-- with p f = map (\client -> if p client then f client else client)








-- find :: [Client] -> Maybe Client

-- delete :: (Client -> Bool) -> [Client] -> [Client]
-- delete f (c:cs)
--     | f c = cs
--     | otherwise = c : delete f cs
-- delete _ _ = []


-- focus :: (Client -> Bool) -> Queue -> Queue
-- focus f (Queue a Nothing b) = Queue (delete f a) (L.find f (a ++ b)) (delete f b)
-- focus f q@(Queue a (Just c) b)
--     | f c = q
--     | otherwise = Queue a new b
--     where
--     new = safeHead . filter f $ a ++ b
