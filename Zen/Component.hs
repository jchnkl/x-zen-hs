-- vim:sw=4:sts=4:ts=4

module Component where

import Types


runComponent :: Monad m => AnyEvent -> Component m -> m (Component m)
runComponent (AnyEvent e) (Component cid d execc su sd handlers) = do
    d' <- execc (mapM_ dispatchEvent $ handlers d) d
    return $ Component cid d' execc su sd handlers
    where dispatchEvent (SomeHandler h) = dispatch h e


startupComponent :: Component c -> c (Component c)
startupComponent (Component cid d r startup su h) = do
    d' <- startup d
    return $ Component cid d' r startup su h


shutdownComponent :: Component c -> c ()
shutdownComponent (Component _ d _ _ shutdown _) = shutdown d
