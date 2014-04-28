{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, DeriveFunctor #-}

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
    { lens_peekat :: a -> b -- ^ getter
    , lens_adjust :: b -> a -> a -- ^ setter
    }

data Getter a b = Getter
    { getter_peekat :: a -> b
    }

data Setter a b = Setter
    { setter_adjust :: b -> a -> a
    }


class LensLike l a b where
    peekat :: l a b -> a -> b
    adjust :: l a b -> b -> a -> a

instance LensLike Lens a b where
    peekat = lens_peekat
    adjust = lens_adjust


class Gettable l a b where
    view :: l a b -> a -> b

instance Gettable Lens a b where
    view = peekat

instance Gettable Getter a b where
    view = getter_peekat


class Settable l a b where
    alter :: l a b -> b -> a -> a

instance Settable Lens a b where
    alter = adjust

instance Settable Setter a b where
    alter = setter_adjust


class Composable l a b where
    compose :: l b c -> l a b -> l a c

instance Composable Lens a b where
    compose l r = lens (peekat l . peekat r) (\a c -> adjust r (adjust l a (peekat r c)) c)

instance Composable Getter a b where
    compose l r = Getter (view l . view r)


-- instance Lens l a b => Composable l a b where
--     compose l r = lens (peekat l . peekat r) (\a c -> adjust r (adjust l a (peekat r c)) c)


-- | Lens constructor as function
lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens = Lens

-- | Lens constructor as function
getter :: (a -> b) -> Getter a b
getter = Getter

-- | Lens constructor as function
setter :: (b -> a -> a) -> Setter a b
setter = Setter


-- instance Lens l a b => Category l where
--     -- (.) :: C b c -> C a b -> C a c
--     (.) l r = lens (peekat l . peekat r) (\a c -> adjust r (adjust l a (peekat r c)) c)
--     -- id :: C a a
--     id = lens id const

-- \c b -> b { _c = c }
-- \b a -> a { _b = b }
-- \c a -> a { _b = _b { _c = c } }

-- (c -> b -> b) -> (b -> a -> a) -> (c -> a -> a)
-- Setter b c -> Setter a b -> Setter a c

-- | Infix Lens combinater. Like @.@, but for lenses
(<.>) :: Composable l a b => l b c -> l a b -> l a c
(<.>) = compose
infixr 9 <.>

transform :: Gettable l a b => (b -> c) -> l a b -> Getter a c
transform f l = getter $ f . view l

(<%>) :: Gettable l a b => (b -> c) -> l a b -> Getter a c
(<%>) = transform
infixl 8 <%>

mutate :: Settable l a b => l a b -> (b -> b) -> Setter a b
mutate l f = setter $ alter l . f

(<~>) :: Settable l a b => l a b -> (b -> b) -> Setter a b
(<~>) = mutate
infixl 8 <~>


getL :: (Monad m, MonadState a m, Gettable l a b) => l a b -> m b
getL l = get >>= return . view l

putL :: (Monad m, MonadState a m, Settable l a b) => l a b -> b -> m ()
putL l v = get >>= put . alter l v

-- | infix putL
(^:=) :: (Monad m, MonadState a m, Settable l a b) => l a b -> b -> m ()
l ^:= v = putL l v
infix 1 ^:=

-- | @convert@ for @MonadState@
getsL :: (Monad m, MonadState a m, Gettable l a b) => l a b -> (b -> c) -> m c
getsL l f = liftM f (getL l)

-- | mutate for Lenses
modifyL' :: (Monad m, MonadState a m, Gettable l a b) => l a b -> Setter a b -> m ()
modifyL' l s = modify (\v -> alter s (view l v) v)

-- | mutate for Lenses
modifyL :: (Monad m, MonadState a m, LensLike l a b) => l a b -> (b -> b) -> m ()
modifyL l f = get >>= \v -> put $ adjust l (f (peekat l v)) v

-- | infix modifyL
(%:=) :: (Monad m, MonadState a m, LensLike l a b) => l a b -> (b -> b) -> m ()
l %:= f = modifyL l f
infix 1 %:=

-- | getL l >>= f
($*>) :: (Monad m, MonadState a m, Gettable l a b) => l a b -> (b -> m c) -> m c
($*>) l f = getL l >>= f
infixl $*>

-- | f =<< getL l
(<*$) :: (Monad m, MonadState a m, Gettable l a b) => (b -> m c) -> l a b -> m c
(<*$) = flip ($*>)
infixr <*$

-- | @convert@ for @MonadReader@
askL :: (Monad m, MonadReader a m, Gettable l a b) => l a b -> m b
askL l = ask >>= return . view l

-- | @convert@ for @MonadReader@
asksL :: (Monad m, MonadReader a m, Gettable l a b) => l a b -> (b -> c) -> m c
asksL l f = liftM f (askL l)

-- | askL l >>= f
($->) :: (Monad m, MonadReader a m, Gettable l a b) => l a b -> (b -> m c) -> m c
($->) l f = askL l >>= f
infixl 8 $->

-- | f =<< askL l
(<-$) :: (Monad m, MonadReader a m, Gettable l a b) => (b -> m c) -> l a b -> m c
(<-$) = flip ($->)
infixr 8 <-$



-- |
-- lmap :: (Monad m, MonadReader a m, Gettable l a b) => (b -> m c) -> l a b -> m c
-- lmap f l = asks (view l) >>= f
-- infixl 8 $->

{-
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

-- transform :: (b -> c) -> Lens a b -> OneWayLens a c
-- transform f l = OneWayLens (f . peekat l)

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

-- | infix @asksL@ from left
(&->) :: (Monad m, MonadReader a m) => (b -> c) -> Lens a b -> m c
(&->) = flip asksL
infixl 8 &->

-- -- | infix @asksL@ from right
-- (<-%) :: (Monad m, MonadReader a m) => Lens a b -> (b -> c) -> m c
-- (<-%) = asksL
-- infixr 8 <-%

-- |
lmap :: (Monad m, MonadReader a m, Viewer l a b) => (b -> m c) -> l a b -> m c
lmap f l = asks (view l) >>= f
-- infixl 8 $->

-- | infix asksL from left
($->) :: (Monad m, MonadReader a m) => Lens a b -> (b -> m c) -> m c
($->) l f = asks (peekat l) >>= f
infixl 8 $->

-- | infix asksL from right
(<-$) :: (Monad m, MonadReader a m) => (b -> m c) -> Lens a b -> m c
(<-$) = flip ($->)
infixr 8 <-$
-}

-- Test

data Position = Position
    { _x :: Int
    , _y :: Int
    }
    deriving (Eq, Read, Show)

x :: Lens Position Int
x = lens _x (\v d -> d { _x = v })

y :: Lens Position Int
y = lens _y (\v d -> d { _y = v })


data Dimension = Dimension
    { _width :: Int
    , _height :: Int
    }
    deriving (Eq, Read, Show)

width :: Lens Dimension Int
width = lens _width (\v d -> d { _width = v })

height :: Lens Dimension Int
height = lens _height (\v d -> d { _height = v })


data Geometry = Geometry
    { _position :: Position
    , _dimension :: Dimension
    }
    deriving (Eq, Read, Show)

position :: Lens Geometry Position
position = lens _position (\v d -> d { _position = v })

dimension :: Lens Geometry Dimension
dimension = lens _dimension (\v d -> d { _dimension = v })

type GeometryT = StateT Geometry IO

geom :: Geometry
geom = Geometry (Position 1 2) (Dimension 3 4)

runG :: IO ()
runG = runStateT gCore geom >>= print

gCore :: GeometryT ()
gCore = get >>= liftIO . print
