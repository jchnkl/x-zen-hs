-- {-# LANGUAGE TemplateHaskell #-}

-- vim macro for lens function type
-- mx{jwyw'xdf_2f "0PxbPaLens j0

-- vim macro for lens function
-- dt_ywPa= lens pa(\v d = d { f:C= v })0x

import Prelude hiding ((.), id)
import Data.Word
import Control.Category
import Control.Monad.State
-- import Data.Lens.Common
-- import Control.Lens ((-=))

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

getL' :: (MonadState a m, Monad m) => Lens a b -> m b
getL' l = get >>= return . getL l

setL' :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
setL' l v = get >>= put . setL l v

(^:=) :: (MonadState a m, Monad m) => Lens a b -> b -> m ()
l ^:= v = setL' l v
infix 4 ^:=

modL' :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
modL' l f = get >>= put . modL l f

(%:=) :: (MonadState a m, Monad m) => Lens a b -> (b -> b) -> m ()
l %:= f = modL' l f
infix 4 %:=

-- (~=) :: (MonadState a m, Monad m) => Lens a b -> m b -> m a -> m a
-- (~=) l v = get >>= return . getL l


lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens = Lens

data Position = Position { _x :: Int, _y :: Int }
    deriving (Eq, Read, Show)

x :: Lens Position Int
x = lens _x (\x' p -> p { _x = x' })

y :: Lens Position Int
y = lens _y (\y' p -> p { _y = y' })

-- makeLenses ''Position

data Dimension = Dimension { _width :: Word, _height :: Word }
    deriving (Eq, Read, Show)

width :: Lens Dimension Word
width = lens _width (\width' p -> p { _width = width' })

height :: Lens Dimension Word
height = lens _height (\height' p -> p { _height = height' })

-- makeLenses ''Dimension

data Geometry = Geometry { _position :: Position, _dimension :: Dimension }
    deriving (Eq, Read, Show)

position :: Lens Geometry Position
position = lens _position (\position' p -> p { _position = position' })

dimension :: Lens Geometry Dimension
dimension = lens _dimension (\dimension' p -> p { _dimension = dimension' })

-- makeLenses ''Geometry

-- position :: Functor f => (Position -> f Position) -> Geometry -> f Geometry
-- position f (Geometry position' geometry') = fmap (\position'' -> Geometry position'' geometry') (f position')

-- x :: Functor f => (Int -> f Int) -> Position -> f Position
-- x f (Position x' y') = fmap (\x'' -> Position x'' y') (f x')

-- -- quux :: Lens (Foo a) (Foo b) a b
-- quux :: Functor f => (a -> f b) -> Foo a -> f (Foo b)
-- quux f (Foo a b c) = fmap (Foo a b) (f c)


runTestLens :: StateT Geometry IO ()
runTestLens = do

    (x . position) %:= (+1)
    (width . dimension) %:= (+100)
    getL' (x . position) >>= liftIO . print


    -- position . x -= 1
    -- position . y -= 1
    -- dimension . width += 100
    -- dimension . height += 100
    get >>= liftIO . putStrLn . ("runTestLens: " ++) . show
    -- doSomethingElse

-- doSomethingElse :: StateT Geometry IO ()
-- doSomethingElse = do
--     position . x ^. (+ 1) >>= liftIO . print
--     -- position ^= Position 13 42

initialGeometry :: Geometry
initialGeometry = Geometry (Position 1 2) (Dimension 100 200)

test :: IO ()
test = do
    let xGeometry = _x . _position
    let x' = xGeometry initialGeometry
    print x'
    execStateT runTestLens initialGeometry >>= print
