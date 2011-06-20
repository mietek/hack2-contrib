module Hack2.Contrib.Middleware.UTF8Body (utf8_body) where

import Air.Env hiding (Default, def)
import Prelude ()
import Air.Heavy
import Hack2
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy.UTF8 (fromString)

utf8_body :: Middleware
utf8_body app = \env -> do
  r <- app env
  let raw_body = r.body
  return r {body = raw_body.unpack.unescape_xml.fromString}
