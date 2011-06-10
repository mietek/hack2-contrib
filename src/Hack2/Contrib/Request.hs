{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Hack2.Contrib.Request where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe
import Hack2 hiding (body)
import Hack2.Contrib.Constants
import Hack2.Contrib.Utils
import Air.Env
import Network.CGI.Cookie
import Network.CGI.Protocol
import Prelude ()
import qualified Data.ByteString.Lazy.Char8 as B
import Hack2.Contrib.AirBackports


body :: Env -> ByteString
body = hack_input

scheme :: Env -> ByteString
scheme = hack_url_scheme > show > lower > B.pack

port :: Env -> Int
port = server_port

path :: Env -> ByteString
path env = env.script_name + env.path_info

content_type :: Env -> ByteString
content_type env = env.httpHeaders.get _ContentType .fromMaybe ""

media_type :: Env -> ByteString
media_type env = case env.content_type.B.unpack.split "\\s*[;,]\\s*" of
  [] -> ""
  x:_ -> x.lower.B.pack


media_type_params :: Env -> [(ByteString, ByteString)]
media_type_params env
  | env.content_type.B.unpack.empty = []
  | otherwise = 
      env
        .content_type
        .B.unpack
        .split "\\s*[;,]\\s"
        .drop 1
        .map (split "=")
        .select (length > is 2)
        .map tuple2
        .map_fst (lower > B.pack)
        .map_snd (B.pack)

content_charset :: Env -> ByteString
content_charset env = env.media_type_params.lookup "charset" .fromMaybe ""

host :: Env -> ByteString
host env = env.httpHeaders.get _Host .fromMaybe (env.server_name) .B.unpack.gsub ":\\d+\\z" "" .B.pack


params :: Env -> [(ByteString, ByteString)]
params env =
  if env.query_string.B.unpack.empty
    then []
    else env.query_string.B.unpack.formDecode.map_both B.pack

inputs :: Env -> [(ByteString, ByteString)]
inputs env = 
  env
    .httpHeaders
    .map_fst (B.unpack > upper > gsub "-" "_") -- cgi env use all cap letters
    .map_snd B.unpack
    .(("REQUEST_METHOD", env.request_method.show) : ) -- for cgi request
    .flip decodeInput (env.body)
    .fst
    .concatMap to_headers
  where
    to_headers (k, input) = case input.inputFilename of
      Nothing -> [(k.B.pack, input.inputValue)]
      Just name -> 
        [  (k.B.pack, input.inputValue)
        ,  ("hack2_input_file_name_" + k.B.pack, name.B.pack)
        ]

referer :: Env -> ByteString
referer = httpHeaders > get _Referer > fromMaybe "/"

cookies :: Env -> [(ByteString, ByteString)]
cookies env = case env.httpHeaders.get _Cookie of
  Nothing -> []
  Just s -> s.B.unpack.readCookies .map_both B.pack

fullpath :: Env -> ByteString
fullpath env = 
  if env.query_string.B.unpack.empty 
    then env.path 
    else env.path + "?" + env.query_string

set_http_header :: ByteString -> ByteString -> Env -> Env
set_http_header k v env = env {httpHeaders = env.httpHeaders.put k v}

set_hack_header :: ByteString -> ByteString -> Env -> Env
set_hack_header k v env = env {hackHeaders = env.hackHeaders.put k v}

url :: Env -> ByteString
url env =
  [  env.scheme
  ,  "://"
  ,  env.host
  ,  port_string
  ,  env.fullpath
  ]
  .B.concat
  where
    port_string = 
      if (env.scheme.is "https" && env.port.is_not 443 || 
          env.scheme.is "http" && env.port.is_not 80 )
        then ":" + env.server_port.show_bytestring
        else ""


remote_host :: Env -> ByteString
remote_host env = 
  ( env.hackHeaders + env.httpHeaders ) .lookup "RemoteHost" .fromMaybe ""