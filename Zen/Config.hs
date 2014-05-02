module Config where

import Data.Map as M
import Graphics.XHB
import Graphics.X11.Types -- hiding (Connection, EventMask)

import Log
import Types
import Pointer


defaultConfig :: Config
defaultConfig = Config
    -- { _modMask = [ModMask1, ModMaskControl]
    { _modMask = [ModMask1]
    , _borderWidth = 3
    , _normalBorderColor = 0x00a0a0a0
    , _focusedBorderColor = 0x00ffce28
    , _selectionBorderColor = 0x00ff0000

    , _keyHandler = M.fromList
        -- [ (([], xK_Alt_L), InputHandler
        --     { press = handleKeyPress2 -- \_ -> (toLog ("[], xK_a" ))
        --     , release = handleKeyRelease2 -- \_ -> (toLog ("[], xK_a"))
        --     } )

        [ (([], xK_a), InputHandler
            { press = \_ -> (toLog ("[], xK_a" ))
            , release = \_ -> (toLog ("[], xK_a"))
            } )

        , (([ModMaskShift], xK_a), InputHandler
            { press = \_ -> (toLog ("[ModMaskShift], xK_a" ))
            , release = \_ -> (toLog ("[ModMaskShift], xK_a"))
            -- { press = io . putStrLn . ("[ModMaskShift], xK_a: " ++ ) . show
            -- , release = io . putStrLn . ("[ModMaskShift], xK_a: " ++ ) . show
            } )
        ]

    , _buttonHandler = M.fromList
        [ (([], ButtonIndex1), moveWindowHandler)
        , (([], ButtonIndex2), resizeWindowHandler)
        , (([], ButtonIndex3), lowerWindowHandler)
        , (([ModMaskShift], ButtonIndex3), raiseWindowHandler)
        ]
    }
