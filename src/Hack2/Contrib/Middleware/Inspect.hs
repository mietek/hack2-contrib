-- | print the env and response in the console

module Hack2.Contrib.Middleware.Inspect (inspect) where

import Hack2
import Air.Light
import Prelude hiding ((.), (^))

inspect :: Middleware
inspect app = \env -> env.trace'.app ^ trace'
