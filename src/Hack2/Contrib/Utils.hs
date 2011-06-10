{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hack2.Contrib.Utils where

import Control.Arrow ((<<<))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Default
import Data.List (lookup)
import Data.Time
import Hack2
import Hack2.Contrib.Constants
import Air.Light
import Network.URI hiding (path)
import Prelude hiding ((.), (^), (>), lookup, (+), (/), (-))
import System.Locale (defaultTimeLocale)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Hack2.Contrib.AirBackports

empty_app :: Application
empty_app = return def

-- | usage: app.use [content_type, cache]
use :: [Middleware] -> Middleware
use = reduce (<<<)

-- use the get / put helper to deal with headers
put :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
put k v xs = (k,v) : xs.reject (fst > is k)

get :: (Eq a) => a -> [(a, b)] -> Maybe b
get = lookup

bytesize :: ByteString -> Int
bytesize = B.length > from_i

show_bytestring :: (Show a) => a -> ByteString
show_bytestring = show > B.pack

map_both :: (a -> b) -> [(a,a)] -> [(b,b)]
map_both f = map_fst f > map_snd f

as_string :: (String -> String) -> ByteString -> ByteString
as_string f x = x.B.unpack.f.B.pack

dummy_middleware :: Middleware
dummy_middleware = id

dummy_app :: Application
dummy_app _ = return - def { status = 500 }

escape_html :: String -> String
escape_html = concatMap fixChar
  where
    fixChar '&'   = "&amp;"
    fixChar '<'  = "&lt;"
    fixChar '>'  = "&gt;"
    fixChar '\'' = "&#39;"
    fixChar '"'  = "&quot;"
    fixChar x    = [x]

escape_uri :: String -> String
escape_uri = escapeURIString isAllowedInURI

unescape_uri :: String -> String
unescape_uri = unEscapeString

show_status_message :: Int -> Maybe ByteString
show_status_message x = status_code.M.lookup x


httpdate :: UTCTime -> String
httpdate x = x.format_time "%a, %d %b %Y %X GMT"

request_method    :: Env -> RequestMethod
script_name       :: Env -> ByteString
path_info         :: Env -> ByteString
query_string      :: Env -> ByteString
server_name       :: Env -> ByteString
server_port       :: Env -> Int
hack_version      :: Env -> (Int, Int, Int)
hack_url_scheme   :: Env -> HackUrlScheme
hack_input        :: Env -> ByteString
hack_errors       :: Env -> HackErrors
hack_headers       :: Env -> [(ByteString, ByteString)]

request_method  = requestMethod
script_name     = scriptName
path_info       = pathInfo
query_string    = queryString
server_name     = serverName
server_port     = serverPort
hack_version    = hackVersion
hack_url_scheme = hackUrlScheme
hack_input      = hackInput
hack_errors     = hackErrors
hack_headers    = hackHeaders

