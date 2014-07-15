-- vim:sw=4:sts=4:ts=4

module Config where

import Data.Map as M
import Graphics.XHB
import Graphics.X11.Types -- hiding (Connection, EventMask)

import Log
import Lens
import Types hiding (Move, Resize, pointer)

import Base
import Core (KeyEventHandler(..), CoreConfig(..))
import qualified Core as C
import qualified Queue as Q
import Button


coreConfig :: CoreConfig
coreConfig = CoreConfig M.empty
    -- [ (([], xK_Tab), C.defaultKeyEventHandler
    --     { press = const $ do
    --         toLog "xK_Tab press"
    --         modifyL C.queue $ Q.focusNext
    --         C.refresh
    --     }
    --   )
    -- ]

core :: Component
core = C.core coreConfig

buttons :: ButtonConfig
buttons = ButtonConfig $ M.fromList
    [ (([],             ButtonIndex1), Move)
    , (([],             ButtonIndex2), Resize)
    , (([],             ButtonIndex3), Lower)
    , (([ModMaskShift], ButtonIndex3), Raise)
    ]

pointer :: Component
pointer = pointerComponent buttons

defaultConfig :: Config
defaultConfig = Config
    { _modMask = [ModMask1]
    , _borderWidth = 3
    , _normalBorderColor = 0x00a0a0a0
    , _focusedBorderColor = 0x00ffce28
    , _selectionBorderColor = 0x00ff0000

    , _components = [base, core, pointer]
    }
