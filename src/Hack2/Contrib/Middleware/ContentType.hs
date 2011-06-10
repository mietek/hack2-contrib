-- | Stolen from rack: Sets the Content-Type header on responses which don't 
--   have one.

module Hack2.Contrib.Middleware.ContentType (content_type) where

import Hack2
import Hack2.Contrib.Constants
import Hack2.Contrib.Response
import Air.Light
import Prelude hiding ((.), (^), (>), (-))
import Data.ByteString.Lazy.Char8 (ByteString)


content_type :: ByteString -> Middleware
content_type s app = \env -> do
  response <- app env
  
  return - case response.header _ContentType of
    Nothing -> response.set_header _ContentType s
    Just _ -> response
