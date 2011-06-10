module Hack2.Contrib.Middleware.Head (head) where

import Hack2
import Hack2.Contrib.Response
import Hack2.Contrib.Utils
import Air.Light
import Prelude hiding ((.), (^), (>), head)
import qualified Data.ByteString.Lazy.Char8 as B


head :: Middleware
head app = \env -> do
  response <- app env
  if env.request_method.is HEAD 
    then response .set_body B.empty .set_content_length 0 .return
    else response .return
