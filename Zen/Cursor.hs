module Cursor where

import Data.Map as M
import Control.Exception (bracket)
import Graphics.XHB hiding (Setup)
import Graphics.X11.Xlib.Font (Glyph)

import Lens
import Util
import Types


lookupCursor :: Glyph -> Z CURSOR
lookupCursor glyph = asksL glyphCursors (M.findWithDefault (fromXid xidNone) glyph)


changeCursor :: CURSOR -> Z ()
changeCursor cursor = connection $-> \c ->
    askL buttonMask >>= io . changeActivePointerGrab c . changegrab
    where
    changegrab = MkChangeActivePointerGrab cursor (toValue TimeCurrentTime)


withFont :: Connection -> String -> (FONT -> IO b) -> IO b
withFont c name = bracket getFont (closeFont c)
    where
    getFont :: IO FONT
    getFont = do
        font <- newResource c :: IO FONT
        openFont c $ MkOpenFont font (fi $ length name) (stringToCList name)
        return font


withGlyphCursor :: Connection -> FONT -> Glyph -> (CURSOR -> IO a) -> IO a
withGlyphCursor c font glyph = bracket acquireCursor (freeCursor c)
    where
    source_char = fi glyph

    acquireCursor :: IO CURSOR
    acquireCursor = do
        cursor <- newResource c :: IO CURSOR
        createGlyphCursor c $ MkCreateGlyphCursor cursor font font
                                                  source_char (source_char + 1)
                                                  0 0 0 0xffff 0xffff 0xffff
        return cursor


withGlyphCursors :: Connection
                 -> FONT
                 -> [Glyph]
                 -> (Map Glyph CURSOR -> IO a)
                 -> IO a
withGlyphCursors c font = run M.empty
    where
    run cursors (glyph:glyphs) f =
        withGlyphCursor c font glyph $ \cursor ->
            run (M.insert glyph cursor cursors) glyphs f
    run cursors _ f = f cursors
