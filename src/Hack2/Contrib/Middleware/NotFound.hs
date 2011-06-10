module Hack2.Contrib.Middleware.NotFound (not_found) where

import Hack2
import Hack2.Contrib.Response
import Hack2.Contrib.Constants

import Air.Light
import Prelude hiding ((.), (^), (>), (+))
import Data.Default


not_found :: Middleware
not_found _ = \_ -> return $
  def
    .set_status 404
    .set_content_type _TextHtml
    .set_content_length 0
