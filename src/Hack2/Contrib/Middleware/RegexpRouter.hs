-- | matching a list of regexp agains a path_info, if matched, consponding app
--   is used, otherwise, pass the env down to lower middleware

module Hack2.Contrib.Middleware.RegexpRouter (regexp_router) where

import Data.Maybe
import Hack2
import Hack2.Contrib.Utils
import Hack2.Contrib.AirBackports
import List (find)
import Air
import Prelude hiding ((.), (^), (>), (-))
import qualified Data.ByteString.Char8 as B

type RoutePath = (String, Application)

regexp_router :: [RoutePath] -> Middleware
regexp_router h app = \env ->
  let path = env.path_info.B.unpack
  in
  case h.find (fst > flip match path > isJust) of
    Nothing -> app env
    Just (_, found_app) -> found_app env