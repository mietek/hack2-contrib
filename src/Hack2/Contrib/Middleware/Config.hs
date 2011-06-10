-- | Stolen from rack-contrib: modifies the environment using the block given 
--   during initialization.

module Hack2.Contrib.Middleware.Config (config) where

import Hack2

config :: (Env -> Env) -> Middleware
config alter app = \env -> app (alter env)