-- vim:sw=4:sts=4:ts=4

module XcbEventSource where

import Graphics.XHB (waitForEvent)

import Lens
import Util
import Types


xcbEventSource :: SetupRT IO AnyEvent
xcbEventSource = askL connection >>= fmap AnyEvent . io . waitForEvent
