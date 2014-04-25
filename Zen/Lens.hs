{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Lens
    ( Lens(..)
    -- , (.)
    -- , id
    , lens
    , (<.>)
    , (^.)
    , (^=)
    , convert
    , mutate
    , askL
    , getL
    , putL
    , (^:=)
    , modifyL
    , (%:=)
    , getsL
    , ($*>)
    , (<*$)
    , asksL
    , ($->)
    , (<-$)
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
    { inspect :: a -> b -- ^ getter
    , adjust :: b -> a -> a -- ^ setter
    }

instance Category Lens where
    -- (.) :: C b c -> C a b -> C a c
    (.) l r = Lens (inspect l . inspect r) (\a c -> adjust r (adjust l a (inspect r c)) c)
    -- id :: C a a
    id = Lens id const

-- | Lens constructor as function
lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens = Lens

-- | Infix Lens combinater. Like @.@, but for lenses
(<.>) :: Lens b c -> Lens a b -> Lens a c
(<.>) = (.)
infixr 9 <.>

-- | Infix getter. flip inspect
(^.) :: a -> Lens a b -> b
(^.) = flip inspect
infixl 8 ^.

-- | Infix setter. adjust
(^=) :: Lens a b -> b -> a -> a
(^=) = adjust
infix 8 ^=

-- | inspect with a projection function
convert :: Lens a b -> (b -> c) -> a -> c
convert l f = f . (inspect l)

-- | Functional modifier through a Lens
mutate :: Lens a b -> (b -> b) -> a -> a
mutate l f v = adjust l (f $ inspect l v) v

-- | Monadic variants
-- get for Lenses makes no sense, only zooming in with gets is supported
-- Named after monadic functions with an L suffix

-- | ask for Lenses
askL :: (MonadReader a m, Monad m) => Lens a b -> m b
askL l = liftM (inspect l) ask

-- | get for Lenses
getL :: (MonadState a m, Monad m) => Lens a b -> m b
getL l = liftM (inspect l) get

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
getsL :: (Monad m, MonadState a m) => Lens a b -> (b -> m c) -> m c
getsL l f = gets (inspect l) >>= f

-- | infix getsL from left
($*>) :: (Monad m, MonadState a m) => Lens a b -> (b -> m c) -> m c
($*>) = getsL
infixl 8 $*>

-- | infix getsL
(<*$) :: (Monad m, MonadState a m) => (b -> m c) -> Lens a b -> m c
(<*$) = flip getsL
infixr 8 <*$


-- | @convert@ for @MonadReader@
asksL :: (Monad m, MonadReader a m) => Lens a b -> (b -> m c) -> m c
asksL l f = asks (inspect l) >>= f

-- | infix asksL from left
($->) :: (Monad m, MonadReader a m) => Lens a b -> (b -> m c) -> m c
($->) = asksL
infixl 8 $->

-- | infix asksL from right
(<-$) :: (Monad m, MonadReader a m) => (b -> m c) -> Lens a b -> m c
(<-$) = flip asksL
infixr 8 <-$
