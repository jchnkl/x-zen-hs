{-# LANGUAGE DeriveDataTypeable #-}

module Queue where

import Data.Typeable (Typeable)
import Data.Maybe (maybeToList)
import qualified Data.Map as M
import qualified Data.List as L

import Lens
import Util
import Types


empty :: ClientQueue
empty = ClientQueue [] Nothing []


member :: WindowId -> ClientQueue -> Bool
member w (ClientQueue as mc bs) = check $ maybeToList mc ++ as ++ bs
    where check []                     = False
          check (c:cs) | w == c ^. xid = True
                       | otherwise     = check cs


toList :: ClientQueue -> [Client]
toList (ClientQueue as mc bs) = maybeToList mc ++ as ++ bs


fromList :: [(WindowId, Client)] -> ClientQueue
fromList cs = ClientQueue [] (safeHead cs') (safeTail cs')
    where cs' = map snd cs


lookup :: WindowId -> ClientQueue -> Maybe Client
lookup w = find . toList
    where find []                     = Nothing
          find (c:cs) | w == c ^. xid = Just c
                      | otherwise     = find cs


rotateForward :: [a] -> [a] -> ([a], [a])
rotateForward [] []         = ([], [])
rotateForward (a:as) []     = (as, [a])
rotateForward [] (b:bs)     = ([b], bs)
rotateForward (a:as) (b:bs) = (as ++ [b], bs ++ [a])


focusNext :: ClientQueue -> ClientQueue
focusNext (ClientQueue as mc bs) = ClientQueue as'' (safeHead bs'') (safeTail bs'')
    where as'' = fst $ rotateForward (as' mc) bs
          bs'' = snd $ rotateForward (as' mc) bs
          as' (Just c) = (as ++ [c])
          as' Nothing  = as


insert :: Client -> ClientQueue -> ClientQueue
insert client (ClientQueue as Nothing bs)  = ClientQueue as (Just client) bs
insert client (ClientQueue as (Just f) bs) = ClientQueue as (Just client) (f:bs)


deleteFirstBy :: (a -> Bool) -> [a] -> [a]
deleteFirstBy _ [] = []
deleteFirstBy f (a:as)
    | f a = as
    | otherwise = a : deleteFirstBy f as


remove :: WindowId -> ClientQueue -> ClientQueue
remove w (ClientQueue as mc bs)
    | Just c   <- mc = ClientQueue (del as) (focus' c) (below' c)
    | otherwise      = ClientQueue (del as) Nothing (del bs)
    where del      = deleteFirstBy ((w ==) . (^. xid))
          focus' c = if (w == c ^. xid) then safeHead (del bs) else Just c
          below' c = if (w == c ^. xid) then safeTail (del bs) else (del bs)


withClient :: WindowId -> (Client -> a) -> ClientQueue -> Maybe a
withClient w f = exec . toList
    where exec []                       = Nothing
          exec (c:cs) | w == (c ^. xid) = Just $ f c
                      | otherwise       = exec cs

modifyClient :: WindowId -> (Client -> Client) -> ClientQueue -> ClientQueue
modifyClient w f (ClientQueue as mc bs) = ClientQueue (map modify as)
                                                      (fmap modify mc)
                                                      (map modify bs)
    where modify c | w == (c ^. xid) = f c
                   | otherwise       = c
