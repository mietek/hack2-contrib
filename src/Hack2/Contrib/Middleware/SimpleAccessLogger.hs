{-# LANGUAGE OverloadedStrings #-}


module Hack2.Contrib.Middleware.SimpleAccessLogger (simple_access_logger) where

import Data.Maybe
import Hack2
import Hack2.Contrib.Request hiding (referer)
import Hack2.Contrib.Constants
import Hack2.Contrib.Utils
import Hack2.Contrib.AirBackports
import Air.Env
import Data.Maybe
import Prelude ()
import qualified Data.ByteString.Lazy.Char8 as B
import System.IO
import Control.Concurrent

sync_lock :: MVar ()
sync_lock = purify - newMVar ()

jailed :: IO a -> IO a
jailed io = do
  withMVar sync_lock (const io)
  
simple_access_logger :: Maybe (B.ByteString -> IO ()) -> Middleware
simple_access_logger stream app = \env -> do
  r <- app env
  time <- now ^ format_time simple_time_format ^ B.pack
  let
      puts        = stream.fromMaybe default_stream
      
      method      = env.request_method.show_bytestring
      http_status = env.hack_url_scheme.show_bytestring
      access_path = env.fullpath.as_string unescape_uri
      
      fields =
        [ env.remote_host
        , "-"
        , "[" + time + "]"
        , "\"" + method + " " + access_path + " " + http_status + "\""
        , r.status.show_bytestring
        , r.headers.get _ContentLength .fromMaybe "-"
        ]
  
  puts - fields.B.intercalate " "
  return r
  
  where
    default_stream x = jailed - x.B.putStrLn >> hFlush stdout
