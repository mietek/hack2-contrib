-- | Stolen from rack-contrib: Automatically sets the ETag header on all 
--   String bodies

module Hack2.Contrib.Middleware.ETag (etag) where

import Data.Digest.Pure.MD5
import Hack2
import Hack2.Contrib.Constants
import Hack2.Contrib.Response
import Air.Light
import Prelude hiding ((.), (^), (>), (-))



etag :: Middleware
etag app = \env -> do
  r <- app env
  
  if r.has_header _ETag
    then r.return
    else r.set_header _ETag (r.tag) .return
  
  where 
    tag = 
          body
        > md5
        > show
