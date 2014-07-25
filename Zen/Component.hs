-- vim:sw=4:sts=4:ts=4

module Component where

import Types


startupComponent :: Component c -> c (Component c)
startupComponent (Component cid d r startup su h) = do
    d' <- startup d
    return $ Component cid d' r startup su h


shutdownComponent :: Component c -> c ()
shutdownComponent (Component _ d _ _ shutdown _) = shutdown d
