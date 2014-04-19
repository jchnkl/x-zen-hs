module Config where

import Graphics.XHB
import Types
import Event

initialConfig :: Config
initialConfig = Config
    { _eventHandler = handler
    , _borderWidth = 3
    , _modMask = ModMask1
    , _buttonPressHandler = defaultButtonPressHandler
    , _buttonReleaseHandler = defaultButtonReleaseHandler
    }
