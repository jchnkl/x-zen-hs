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
getL :: (MonadState a1 m, MonadTrans t, Monad (t m))
     => FoldLike r a1 a' r b' -> t m r
getL l = view l `liftM` (lift get)

-- | Control.Monad.State.put
putL :: (MonadState a m, MonadTrans t, Monad (t m))
     => ASetter a a b b' -> b' -> t m ()
putL l v = lift $ get >>= put . (l .~ v)

-- | `putL`
(^:=) :: (MonadState a m, MonadTrans t, Monad (t m))
      => ASetter a a b b' -> b' -> t m ()
(^:=) = putL
infix 4 ^:=

-- | Control.Monad.State.gets
getsL :: (MonadState a m, MonadTrans t, Monad (t m))
      => FoldLike b a a' b b' -> (b -> c) -> t m c
getsL l f = (f . view l) `liftM` (lift get)

-- modifyL :: MonadState a m => Lens a b -> (b -> b) -> m ()
modifyL :: (MonadState s m, MonadTrans t, Monad (t m))
        => ASetter s s b b' -> (b -> b') -> t m ()
modifyL l f = lift $ modify (l %~ f)

-- | `modifyL
(%:=) :: (MonadState s m, MonadTrans t, Monad (t m))
      => ASetter s s b b' -> (b -> b') -> t m ()
(%:=) = modifyL
infix 4 %:=

-- | Control.Monad.Reader.ask
askL :: (MonadReader a1 m, MonadTrans t, Monad (t m))
     => FoldLike r a1 a' r b' -> t m r
askL l = view l `liftM` (lift ask)

-- | Control.Monad.Reader.asks
asksL :: (MonadReader a m, MonadTrans t, Monad (t m))
      => FoldLike b a a' b b' -> (b -> c) -> t m c
asksL l f = (f . view l) `liftM` (lift ask)

-- | >>= with lenses for MonadState
($*>) :: (MonadState a m, MonadTrans t, Monad (t m))
      => FoldLike b a a' b b' -> (b -> t m c) -> t m c
($*>) l f = lift (gets $ view l) >>= f
infix 4 $*>

-- | `($*>)`
(<*$) :: (MonadState a m, MonadTrans t, Monad (t m))
      => (b -> t m c) -> FoldLike b a a' b b' -> t m c
(<*$) = flip ($*>)
infix 4 <*$

-- | >>= with lenses for MonadReader
($->) :: (MonadReader a m, MonadTrans t, Monad (t m))
      => FoldLike b a a' b b' -> (b -> t m c) -> t m c
($->) l f = lift (asks $ view l) >>= f
infix 4 $->

-- | `($->)`
(<-$) :: (MonadReader a m, MonadTrans t, Monad (t m))
      => (b -> t m c) -> FoldLike b a a' b b' -> t m c
(<-$) = flip ($->)
infix 4 <-$
