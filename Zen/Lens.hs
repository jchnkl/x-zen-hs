module Lens
    ( Lens(..)
    -- , (.)
    -- , id
    , lens
    , (<.>)
    , (^.)
    , (^=)
    , modL
    , asksL
    , getsL
    , putsL
    , (^:=)
    , modifyL
    , (%:=)
    ) where

-- vim macro for lens function type
-- mx{jwyw'xdf_2f "0PxbPaLens j0

-- vim macro for lens function
-- dt_ywPa= lens pa(\v d = d { f:C= v })0x

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.State
import Control.Monad.Reader

-- | Simple Lens
data Lens a b = Lens
    { getL :: a -> b -- ^ getter
    , setL :: b -> a -> a -- ^ setter
    }

instance Category Lens where
    -- (.) :: C b c -> C a b -> C a c
    (.) l r = Lens (getL l . getL r) (\a c -> setL r (setL l a (getL r c)) c)
    -- id :: C a a
    id = Lens id const

-- | Lens constructor as function
lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens = Lens

-- | Infix Lens combinater. Like @.@, but for lenses
(<.>) :: Lens b c -> Lens a b -> Lens a c
(<.>) = (.)
infixr 9 <.>

-- | Infix getter. flip getL
(^.) :: a -> Lens a b -> b
(^.) = flip getL
infixl 8 ^.

-- | Infix setter. setL
(^=) :: Lens a b -> b -> a -> a
(^=) = setL
infix 4 ^=

-- | Functional modifier through a Lens
modL :: Lens a b -> (b -> b) -> a -> a
modL l f v = setL l (f $ getL l v) v


-- | Monadic variants
-- get for Lenses makes no sense, only zooming in with gets is supported
-- Named after monadic functions with an L suffix

-- | asks for Lenses
asksL :: (MonadReader a m, Monad m) => Lens a b -> m b
asksL l = liftM (getL l) ask

-- | gets for Lenses
getsL :: (MonadState a m, Monad m) => Lens a b -> m b
getsL l = liftM (getL l) get

-- | put for Lenses
putsL :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
putsL l v = get >>= put . setL l v

-- | infix putsL
(^:=) :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
l ^:= v = putsL l v
infix 4 ^:=

-- | modify for Lenses
modifyL :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
modifyL l f = get >>= put . modL l f

-- | infix modify
(%:=) :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
l %:= f = modifyL l f
infix 4 %:=
