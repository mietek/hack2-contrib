{-# LANGUAGE OverloadedStrings #-}

module Hack2.Contrib.Middleware.XForwordedForToRemoteHost (x_forwarded_for_to_remote_host) where

import Hack2
import Air.Env
import Prelude ()


x_forwarded_for_to_remote_host :: Middleware
x_forwarded_for_to_remote_host app = \env ->
  case env.httpHeaders.lookup "x-forwarded-for" of
    Nothing -> app env
    Just ip -> 
      let newHackHeaders = ("RemoteHost", ip) : env.hackHeaders
      in
      
      app - env {hackHeaders = newHackHeaders}