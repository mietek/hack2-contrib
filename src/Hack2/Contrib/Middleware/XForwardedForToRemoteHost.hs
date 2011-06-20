{-# LANGUAGE OverloadedStrings #-}

module Hack2.Contrib.Middleware.XForwardedForToRemoteHost (x_forwarded_for_to_remote_host) where

import Hack2
import Air.Env hiding (Default, def)
import Prelude ()
import Data.ByteString.Char8 ()

x_forwarded_for_to_remote_host :: Middleware
x_forwarded_for_to_remote_host app = \env ->
  case env.httpHeaders.lookup "x-forwarded-for" of
    Nothing -> app env
    Just ip -> 
      let newHackHeaders = ("RemoteHost", ip) : env.hackHeaders
      in
      
      app - env {hackHeaders = newHackHeaders}