-- Censor it !

module Hack2.Contrib.Middleware.Censor (censor) where

import Hack2

censor :: (Response -> IO Response) -> Middleware
censor alter app = \env -> app env >>= alter