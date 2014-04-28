{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, DeriveFunctor, FunctionalDependencies #-}

module Lens where
    -- ( Lens(..)
    -- -- , (.)
    -- -- , id
    -- , lens
    -- , (<.>)
    -- , (^.)
    -- , (^=)
    -- , convert
    -- , mutate
    -- , askL
    -- , getL
    -- , putL
    -- , (^:=)
    -- , modifyL
    -- , (%:=)
    -- , getsL
    -- , ($*>)
    -- , (<*$)
    -- , asksL
    -- , ($->)
    -- , (<-$)
    -- ) where

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
    { peekat :: a -> b -- ^ getter
    , adjust :: b -> a -> a -- ^ setter
    }

-- data Projection a b = Projection
--     { project :: a -> b -- ^ getter
--     }

-- class Projection l a b | a -> b where
--     project :: l a b -> a -> b

-- instance Functor (Projection a) where
--     -- (a -> b) -> f a -> f b
--     -- (b -> c) -> Lens a b -> Lens a c
--     -- fmap f (Lens p _) = Lens (f . p) undefined
--     fmap f (Projection p) = Projection (f . p)
--     -- fmap = undefined

-- instance Projection Lens a b where
--     project = peekat

instance Category Lens where
    -- (.) :: C b c -> C a b -> C a c
    (.) l r = Lens (peekat l . peekat r) (\a c -> adjust r (adjust l a (peekat r c)) c)
    -- id :: C a a
    id = Lens id const

-- | Lens constructor as function
lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens = Lens

-- | Infix Lens combinater. Like @.@, but for lenses
(<.>) :: Lens b c -> Lens a b -> Lens a c
(<.>) = (.)
infixr 9 <.>

-- | Infix getter. flip peekat
(^.) :: a -> Lens a b -> b
(^.) = flip peekat
infixl 8 ^.

-- | Infix setter. adjust
(^=) :: Lens a b -> b -> a -> a
(^=) = adjust
infix 8 ^=

-- | peekat with a projection function
convert :: Lens a b -> (b -> c) -> a -> c
convert l f = f . (peekat l)

-- TODO: make this work generally Lens a (b -> c) ??
-- | infix @convert@
(^&) :: (b -> c) -> Lens a b -> a -> c
(^&) = flip convert
infixr 8 ^&

-- | Functional modifier through a Lens
mutate :: Lens a b -> (b -> b) -> a -> a
mutate l f v = adjust l (f $ peekat l v) v

-- -- foo :: (b -> c) -> Lens a b -> c
-- foo :: (b -> c) -> (c -> b) -> Lens a b -> Lens a c
-- foo f' f'' l = Lens (f' . peekat l) (adjust l . f'')

-- transform :: (b -> c) -> Lens a b -> Projection a c
-- transform f l = Projection (f . peekat l)

-- | Monadic variants
-- get for Lenses makes no sense, only zooming in with gets is supported
-- Named after monadic functions with an L suffix

-- | ask for Lenses
askL :: (MonadReader a m, Monad m) => Lens a b -> m b
askL l = liftM (peekat l) ask

-- | get for Lenses
getL :: (MonadState a m, Monad m) => Lens a b -> m b
getL l = liftM (peekat l) get

-- | adjust for Lenses
putL :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
putL l v = get >>= put . adjust l v

-- | infix putL
(^:=) :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
l ^:= v = putL l v
infix 1 ^:=


-- | mutate for Lenses
modifyL :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
modifyL l f = get >>= put . mutate l f

-- | infix modifyL
(%:=) :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
l %:= f = modifyL l f
infix 1 %:=


-- | @convert@ for @MonadState@
getsL :: (Monad m, MonadState a m) => Lens a b -> (b -> c) -> m c
getsL l f = liftM f (getL l) -- gets (peekat l) >>= f

-- | infix getsL from left
($*>) :: (Monad m, MonadState a m) => Lens a b -> (b -> m c) -> m c
($*>) l f = gets (peekat l) >>= f
infixl 8 $*>

-- | infix getsL
(<*$) :: (Monad m, MonadState a m) => (b -> m c) -> Lens a b -> m c
(<*$) = flip ($*>)
infixr 8 <*$


-- | @convert@ for @MonadReader@
asksL :: (Monad m, MonadReader a m) => Lens a b -> (b -> c) -> m c
asksL l f = liftM f (askL l)

-- -- | infix @asksL@ from left
-- (&->) :: (Monad m, MonadReader a m) => (b -> c) -> Lens a b -> m c
-- (&->) = flip asksL
-- infixl 8 &->

-- -- | infix @asksL@ from right
-- (<-%) :: (Monad m, MonadReader a m) => Lens a b -> (b -> c) -> m c
-- (<-%) = asksL
-- infixr 8 <-%

-- -- |
-- lmap :: (Monad m, MonadReader a m, Projection l a b) => (b -> m c) -> l a b -> m c
-- lmap f l = asks (project l) >>= f
-- -- infixl 8 $->

-- | infix asksL from left
($->) :: (Monad m, MonadReader a m) => Lens a b -> (b -> m c) -> m c
($->) l f = asks (peekat l) >>= f
infixl 8 $->

-- | infix asksL from right
(<-$) :: (Monad m, MonadReader a m) => (b -> m c) -> Lens a b -> m c
(<-$) = flip ($->)
infixr 8 <-$
