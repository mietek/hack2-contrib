module Hack2.Contrib.Middleware.Cascade (cascade) where

import Prelude ()
import Air.Env hiding (Default, def)
import Hack2
import Data.Default

cascade :: [Application] -> Application
cascade [] = const - return def { status = 404 }
cascade (x:xs) = \env -> do
  r <- x env
  if r.status == 404
    then
      cascade xs env
    else
      return r