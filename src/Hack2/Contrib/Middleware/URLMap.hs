-- | Stolen from rack:
--   Rack::URLMap takes a hash mapping urls or paths to apps, and
--   dispatches accordingly. 
--
--   URLMap modifies the SCRIPT_NAME and PATH_INFO such that the part
--   relevant for dispatch is in the SCRIPT_NAME, and the rest in the
--   PATH_INFO.  This should be taken care of when you need to
--   reconstruct the URL in order to create links.
--   
--   URLMap dispatches in such a way that the longest paths are tried
--   first, since they are most specific.

module Hack2.Contrib.Middleware.URLMap (url_map) where

import Hack2
import Hack2.Contrib.Utils
import List (find, isPrefixOf)
import Air.Env
import Prelude ()
import qualified Data.ByteString.Lazy.Char8 as B


type RoutePath = (B.ByteString, Application)

url_map :: [RoutePath] -> Middleware
url_map h app = \env ->
  let path             = env.path_info
      script           = env.script_name
      mod_env location = env 
        { scriptName  = script + location
        , pathInfo    = path.B.drop (location.B.length)
        }
  in
  case h.find (fst > (`B.isPrefixOf` path) ) of
    Nothing -> app env
    Just (location, app') -> app' (mod_env location)