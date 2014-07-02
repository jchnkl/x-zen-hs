-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Lens
    ( module Lens
    , module Lens.Family
    , module Lens.Family.Stock
    , lens
    ) where

import Lens.Family
import Lens.Family.Stock
import Lens.Family.Unchecked (lens)

import Control.Monad.State
import Control.Monad.Reader

-- | Control.Monad.State.get
getL :: MonadState a1 m => FoldLike r a1 a' r b' -> m r
getL l = view l `liftM` get

-- | Control.Monad.State.put
putL :: MonadState a m => ASetter a a b b' -> b' -> m ()
putL l v = get >>= put . (l .~ v)

-- | `putL`
(^:=) :: MonadState a m => ASetter a a b b' -> b' -> m ()
(^:=) = putL
infix 4 ^:=

-- | Control.Monad.State.gets
getsL :: MonadState a m => FoldLike b a a' b b' -> (b -> c) -> m c
getsL l f = (f . view l) `liftM` get

-- modifyL :: MonadState a m => Lens a b -> (b -> b) -> m ()
modifyL :: MonadState s m => ASetter s s b b' -> (b -> b') -> m ()
modifyL l f = modify (l %~ f)

-- | `modifyL
(%:=) :: MonadState s m => ASetter s s b b' -> (b -> b') -> m ()
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
