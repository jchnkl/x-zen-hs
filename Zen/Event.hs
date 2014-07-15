-- vim:sw=4:sts=4:ts=4

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable,
             ExistentialQuantification,
             MultiParamTypeClasses,
             LambdaCase,
             TupleSections #-}
             -- FlexibleContexts,
             -- RankNTypes,

module Event where

import Data.Typeable
import Data.Word
import Control.Applicative
import Graphics.XHB hiding (Setup)

import Log
import Lens
import Util
import Types
import Window


data BaseComponent = BaseComponent
    deriving (Show, Typeable)

base :: Component
base = baseComponent

baseComponent :: Component
baseComponent = Component
    { componentId = "Base"
    , componentData = BaseComponent
    , ioRunComponent = runBaseComponent
    , onStartup = return . id
    , onShutdown = const $ return ()
    , someHandler = const $
        map SomeHandler $ [ EventHandler handleMapRequest
                          , EventHandler handleConfigureRequest
                          ]
    }


runBaseComponent :: IO a -> BaseComponent -> IO (a, BaseComponent)
runBaseComponent f b = (,b) <$> f


--     -- Structure control events
--     [ EventHandler handleMapRequest
--     , EventHandler handleConfigureRequest
--     , EventHandler handleCirculateNotify
--     -- , EventHandler handleResizeRequest

-- -- Window state notification events
-- , EventHandler handleCreateNotify
-- , EventHandler handleDestroyNotify
-- , EventHandler handleMapNotify
-- , EventHandler handleUnmapNotify

-- -- Window crossing events
-- , EventHandler handleEnterNotify
-- , EventHandler handleLeaveNotify

-- -- Input focus events
-- , EventHandler handleFocusIn
-- , EventHandler handleFocusOut

-- -- Pointer events
-- , EventHandler handleButtonPress
-- , EventHandler handleButtonRelease

-- -- Keyboard events
-- , EventHandler handleKeyPress
-- , EventHandler handleKeyRelease
-- ]


copyValues :: ConfigureRequestEvent -> [ConfigWindow] -> [(ConfigWindow, Word32)]
copyValues e (ConfigWindowX           : ms) =
    (ConfigWindowX,           fi $ x_ConfigureRequestEvent e)               : copyValues e ms
copyValues e (ConfigWindowY           : ms) =
    (ConfigWindowY,           fi $ y_ConfigureRequestEvent e)               : copyValues e ms
copyValues e (ConfigWindowWidth       : ms) =
    (ConfigWindowWidth,       fi $ width_ConfigureRequestEvent e)           : copyValues e ms
copyValues e (ConfigWindowHeight      : ms) =
    (ConfigWindowHeight,      fi $ height_ConfigureRequestEvent e)          : copyValues e ms
copyValues e (ConfigWindowBorderWidth : ms) =
    (ConfigWindowBorderWidth, fi $ border_width_ConfigureRequestEvent e)    : copyValues e ms
copyValues e (ConfigWindowSibling     : ms) =
    (ConfigWindowSibling,     convertXid $ sibling_ConfigureRequestEvent e) : copyValues e ms
copyValues e (ConfigWindowStackMode   : ms) =
    (ConfigWindowStackMode,   toValue $ stack_mode_ConfigureRequestEvent e) : copyValues e ms
copyValues _ _ = []


-- Event handler

handleMapRequest :: MapRequestEvent -> Z IO ()
handleMapRequest e = do
    toLog "MapRequestEvent"
    connection $-> io . flip mapWindow (window_MapRequestEvent e)


handleConfigureRequest :: ConfigureRequestEvent -> Z IO ()
handleConfigureRequest e = do
    toLog "ConfigureRequestEvent"
    configure window values
    where
    window = window_ConfigureRequestEvent e
    values = copyValues e (value_mask_ConfigureRequestEvent e)


handleCirculateNotify :: CirculateNotifyEvent -> Z IO ()
handleCirculateNotify _ = toLog "CirculateNotifyEvent"
