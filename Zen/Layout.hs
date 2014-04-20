{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import GHC.Types (Word)
import Control.Monad (when)

whenIO :: IO Bool -> IO () -> IO ()
whenIO b f = b >>= flip when f

type Window = Word

data Property a = Property
    deriving (Eq, Read, Show)

class WindowProperty a where
    match :: Property a -> Window -> IO Bool

-- data WindowProperty = Name String | Title String | Class String
-- data WindowProperty a = PropertyMatcher a
-- data WindowProperty = WindowProperty
--     deriving (Eq, Read, Show)

type Position = Int
type Dimension = Word

data Geometry = Geometry
    { x :: Position
    , y :: Position
    , width :: Dimension
    , heigth :: Dimension
    }
    deriving (Eq, Read, Show)

data Rule a = Rule
    { property :: Property a
    , geometry :: Geometry
    }
    deriving (Eq, Read, Show)

-- data Layout a = Maybe Layout { rule :: Rule a, layout :: Layout a }
data Layout a = None | Layout { rule :: Rule a, layout :: Layout a }
    deriving (Eq, Read, Show)

data LayoutSet a = LayoutSet
    { current :: Layout a
    , layouts :: [Layout a]
    , windows :: [Window]
    }
    deriving (Eq, Read, Show)


configureWindow :: Geometry -> Window -> IO ()
configureWindow g w = print $ "Configure " ++ show w ++ " to " ++ show g

applyRule :: (WindowProperty a) => Rule a -> Window -> IO ()
applyRule (Rule prop geom) w = whenIO (match prop w) (configureWindow geom w)

applyLayout :: (WindowProperty a) => Layout a -> Window -> IO ()
applyLayout None _ = return ()
applyLayout (Layout r l) w = do
    _ <- applyRule r w
    applyLayout l w

apply :: (WindowProperty a) => LayoutSet a -> IO ()
apply (LayoutSet cl _ ws) = apply' cl ws
    where
    apply' _ [] = return ()
    apply' l (w:wins) = do
        applyLayout cl w
        apply' l wins




instance WindowProperty String where
    -- match string window = True
    match string _ = print string >> return True
