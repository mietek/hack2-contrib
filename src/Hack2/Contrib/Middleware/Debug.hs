-- | print the env and response in the console

module Hack2.Contrib.Middleware.Debug (debug) where

import Hack2

debug :: (Env -> Response -> IO ()) -> Middleware
debug f app = \env -> do
  r <- app env
  f env r
  return r
