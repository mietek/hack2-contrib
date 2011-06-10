module Hack2.Contrib.Middleware.IOConfig (ioconfig) where

import Hack2

ioconfig :: (Env -> IO Env) -> Middleware
ioconfig before app = \env -> before env >>= app