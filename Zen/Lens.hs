module Lens where

-- vim macro for lens function type
-- mx{jwyw'xdf_2f "0PxbPaLens j0

-- vim macro for lens function
-- dt_ywPa= lens pa(\v d = d { f:C= v })0x

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.State
import Control.Monad.Reader

data Lens a b = Lens { getL :: a -> b, setL :: b -> a -> a }

instance Category Lens where
    -- (.) :: C b c -> C a b -> C a c
    (.) l r = Lens (getL l . getL r) (\a c -> setL r (setL l a (getL r c)) c)
    -- id :: C a a
    id = Lens id const

(^.) :: a -> Lens a b -> b
(^.) = flip getL
infixl 8 ^.

(^=) :: Lens a b -> b -> a -> a
(^=) = setL
infix 4 ^=

modL :: Lens a b -> (b -> b) -> a -> a
modL l f v = setL l (f $ getL l v) v



asksL :: (MonadReader a m, Monad m) => Lens a b -> m b
asksL l = liftM (getL l) ask

getsL :: (MonadState a m, Monad m) => Lens a b -> m b
getsL l = liftM (getL l) get

putsL :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
putsL l v = get >>= put . setL l v

(^:=) :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
l ^:= v = putsL l v
infix 4 ^:=

modifyL :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
modifyL l f = get >>= put . modL l f

(%:=) :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
l %:= f = modifyL l f
infix 4 %:=

lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens = Lens
