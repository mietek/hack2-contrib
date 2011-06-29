-- | Stolen from rack: Sets the Content-Length header on responses with 
--   fixed-length bodies.

module Hack2.Contrib.Middleware.ContentLength (content_length) where

import Hack2
import Hack2.Contrib.Constants
import Hack2.Contrib.Response
import Hack2.Contrib.Utils
import Air.Light
import Prelude hiding ((.), (^), (>), (-))

content_length :: Middleware
content_length app = \env -> do
  response <- app env
  
  if should_size response
    then do
      _body <- response.body_bytestring
      let size = _body.bytesize.show_bytestring
      return - 
        response
          .set_header _ContentLength size
          .set_body_bytestring _body
    else 
      return - response
  
  where 
    should_size response =
      [  not - response.has_header _ContentLength
      ,  not - response.has_header _TransferEncoding
      ,  not - status_with_no_entity_body.has(response.status)
      ] .and
