module Hack2.Contrib.Middleware.Head (head) where

import Hack2
import Hack2.Contrib.Response
import Hack2.Contrib.Utils

import Air.Env hiding (def, head)
import Prelude ()
import qualified Data.ByteString.Char8 as B
import Data.Default (def)

head :: Middleware
head app = \env -> do
  response <- app env
  if env.request_method.is HEAD 
    then response .set_body def .set_content_length 0 .return
    else response .return
