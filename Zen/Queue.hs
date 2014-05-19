module Queue where

import qualified Data.Map as M

import Lens
import Util
import Types

insert :: Client -> Queue -> Queue
insert client = M.insert (client ^. xid) client

remove :: WindowId -> Queue -> Queue
remove = M.delete

withClient :: WindowId -> (Client -> a) -> Queue -> Maybe a
withClient w f = fmap f . M.lookup w

-- TODO: more generic combinator?
-- or at least withClient <=> withClientM
-- withClient' :: WindowId -> (Client -> Z a) -> Z (Maybe a)
-- withClient' w f = queue $*> flip whenJust f . M.lookup w 

modifyClient :: WindowId -> (Client -> Client) -> Queue -> Queue
modifyClient w f = M.alter (fmap f) w
