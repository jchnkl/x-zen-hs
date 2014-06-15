module Config where

import Data.Map as M
import Graphics.XHB
import Graphics.X11.Types -- hiding (Connection, EventMask)

import Log
import Lens
import Types

import Event
import Core (KeyEventHandler(..), CoreConfig(..))
import qualified Core as C
import qualified Queue as Q
import Button


coreConfig :: CoreConfig
coreConfig = CoreConfig $ M.fromList
    [ (([], xK_Tab), C.defaultKeyEventHandler
        { press = const $ do
            toLog "xK_Tab press"
            modifyL C.queue $ Q.focusNext
            C.refresh
        }
      )
    ]

core :: Component
core = C.core coreConfig


buttonConfig :: ButtonConfig
buttonConfig = ButtonConfig $ M.fromList
    [ (([],             ButtonIndex1), Move)
    , (([],             ButtonIndex2), Resize)
    , (([],             ButtonIndex3), Lower)
    , (([ModMaskShift], ButtonIndex3), Raise)
    ]


defaultConfig :: Config
defaultConfig = Config
    -- { _modMask = [ModMask1, ModMaskControl]
    { _modMask = [ModMask1]
    , _borderWidth = 3
    , _normalBorderColor = 0x00a0a0a0
    , _focusedBorderColor = 0x00ffce28
    , _selectionBorderColor = 0x00ff0000

    -- , _keyHandler = M.fromList []
        -- [ (([], xK_Alt_L), InputHandler
        --     { press = handleKeyPress2 -- \_ -> (toLog ("[], xK_a" ))
        --     , release = handleKeyRelease2 -- \_ -> (toLog ("[], xK_a"))
        --     } )

--         [ (([], xK_a), InputHandler
--             { press = \_ -> (toLog ("[], xK_a" ))
--             , release = \_ -> (toLog ("[], xK_a"))
--             } )
--
--         , (([ModMaskShift], xK_a), InputHandler
--             { press = \_ -> (toLog ("[ModMaskShift], xK_a" ))
--             , release = \_ -> (toLog ("[ModMaskShift], xK_a"))
--             -- { press = io . putStrLn . ("[ModMaskShift], xK_a: " ++ ) . show
--             -- , release = io . putStrLn . ("[ModMaskShift], xK_a: " ++ ) . show
--             } )
--         ]

    -- , _buttonHandler = M.fromList
    --     [ (([], ButtonIndex1), \_ -> toLog "([], ButtonIndex1)")
    --     , (([], ButtonIndex2), \_ -> toLog "([], ButtonIndex2)")
    --     , (([], ButtonIndex3), \_ -> toLog "([], ButtonIndex3)")
    --     ]

        -- [ (([], ButtonIndex1), moveWindowHandler)
        -- , (([], ButtonIndex2), resizeWindowHandler)
        -- , (([], ButtonIndex3), lowerWindowHandler)
        -- , (([ModMaskShift], ButtonIndex3), raiseWindowHandler)
        -- ]

    , _components = [baseComponent, core, pointerComponent]


    , _componentConfigs = [ComponentConfig buttonConfig]
    }
