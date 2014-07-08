{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module XcbView where

import Data.Map as M
import Data.Set as S
import Data.List as L
import Control.Monad.Reader
import Graphics.XHB as X hiding (Setup)

import Lens
import Util
import Model
import Types

xcbView :: (MonadIO m, MonadReader Setup m) => Model -> UpdateHints -> m ()
xcbView _ = mapM_ (uncurry configure) . M.toList
          . M.map (toValueParam . concat . L.map hintToValueParam . S.toList)
    where
    configure w vp = connection $-> \c -> io $ X.configureWindow c w vp
    hintToValueParam = \case
        HintX v -> [(ConfigWindowX, fi v)]
        HintY v -> [(ConfigWindowY, fi v)]
        HintWidth v -> [(ConfigWindowWidth, fi v)]
        HintHeight v -> [(ConfigWindowHeight, fi v)]
        HintPosition p -> [(ConfigWindowX, fi $ p ^. x), (ConfigWindowY, fi $ p ^. y)]
        HintDimension d -> [(ConfigWindowWidth, fi $ d ^. width), (ConfigWindowHeight, fi $ d ^. height)]
        _ -> []
        -- HintRaise w
        -- HintLower w
        -- HintBorderColor v) w
        -- HintBorderWidth v) w
