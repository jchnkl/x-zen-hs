{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Lens 
    ( module Lens
    , module Lens.Family
    ) where

import Lens.Family

import Control.Monad.State
import Control.Monad.Reader

-- import Prelude hiding ((.), id)
-- import Control.Category

-- | Control.Monad.State.get
getL :: MonadState a1 m => FoldLike r a1 a' r b' -> m r
getL l = view l `liftM` get

-- | Control.Monad.State.put
putL :: MonadState a m => Setter a a b b' -> b' -> m ()
putL l v = get >>= put . (l .~ v)

-- | `putL`
(^:=) :: MonadState a m => Setter a a b b' -> b' -> m ()
(^:=) = putL
infix 4 ^:=

-- | Control.Monad.State.gets
getsL :: MonadState a m => FoldLike b a a' b b' -> (b -> c) -> m c
getsL l f = (f . view l) `liftM` get

-- modifyL :: MonadState a m => Lens a b -> (b -> b) -> m ()
modifyL :: MonadState s m => Setter s s b b' -> (b -> b') -> m ()
modifyL l f = modify (l %~ f)

-- | `modifyL
(%:=) :: MonadState s m => Setter s s b b' -> (b -> b') -> m ()
(%:=) = modifyL
infix 4 %:=

-- | Control.Monad.Reader.ask
askL :: MonadReader a1 m => FoldLike r a1 a' r b' -> m r
askL l = view l `liftM` ask

-- | Control.Monad.Reader.asks
asksL :: MonadReader a m => FoldLike b a a' b b' -> (b -> c) -> m c
asksL l f = (f . view l) `liftM` ask


-- | >>= with lenses for MonadState
($*>) :: MonadState a m => FoldLike b a a' b b' -> (b -> m c) -> m c
($*>) l f = gets (view l) >>= f
infix 4 $*>

-- | `($*>)`
(<*$) :: MonadState a m => (b -> m c) -> FoldLike b a a' b b' -> m c
(<*$) = flip ($*>)
infix 4 <*$

-- | >>= with lenses for MonadReader
($->) :: MonadReader a m => FoldLike b a a' b b' -> (b -> m c) -> m c
($->) l f = asks (view l) >>= f
infix 4 $->

-- | `($->)`
(<-$) :: MonadReader a m => (b -> m c) -> FoldLike b a a' b b' -> m c
(<-$) = flip ($->)
infix 4 <-$



-- -- | ask for Lenses
-- askL :: (MonadReader a m) => Lens a b -> m b
-- askL l = liftM (peekat l) ask

-- -- | get for Lenses
-- getL :: (MonadState a m, Monad m) => Lens a b -> m b
-- getL l = liftM (peekat l) get

-- -- | adjust for Lenses
-- putL :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
-- putL l v = get >>= put . adjust l v

{-
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
-}
