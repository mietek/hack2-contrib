{-# LANGUAGE OverloadedStrings #-}

-- | Stolen from rack-contrib: Bounce those annoying favicon.ico requests

module Hack2.Contrib.Middleware.BounceFavicon (bounce_favicon) where

import Hack2
import Hack2.Contrib.Middleware.NotFound
import Hack2.Contrib.Utils
import Air.Light
import Prelude hiding ((.), (^), (>), head)



bounce_favicon :: Middleware
bounce_favicon app = \env -> do
  if env.path_info.is "/favicon.ico"
    then not_found dummy_app env
    else app env
