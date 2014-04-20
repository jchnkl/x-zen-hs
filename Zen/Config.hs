module Config where

import Graphics.XHB
import Types
import Event

initialConfig :: Config
initialConfig = Config
    { _modMask = ModMask1
    , _borderWidth = 3
    , _normalBorderColor = 0x00a0a0a0
    , _focusedBorderColor = 0x00ffce28
    , _selectionBorderColor = 0x00ff0000
    , _eventHandler = handler
    , _buttonPressHandler = defaultButtonPressHandler
    , _buttonReleaseHandler = defaultButtonReleaseHandler
    }
