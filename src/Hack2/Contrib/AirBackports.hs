module Hack2.Contrib.AirBackports where

import Data.Time
import System.Directory
import System.IO
import System.Locale (defaultTimeLocale)
import qualified System.IO.Unsafe as Unsafe

import Air.Env hiding (Default, def)
import Prelude ()

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as B

b2u, u2b :: String -> String
b2u = B.pack > E.decodeUtf8 > T.unpack
u2b = T.pack > E.encodeUtf8 > B.unpack


file_size :: String -> IO Integer
file_size path = withFile path ReadMode hFileSize

file_mtime :: String -> IO UTCTime
file_mtime = getModificationTime

now :: IO UTCTime
now = getCurrentTime

format_time :: String -> UTCTime -> String
format_time = formatTime defaultTimeLocale

purify :: IO a -> a
purify = Unsafe.unsafePerformIO

simple_time_format :: String
simple_time_format = "%Y-%m-%d %H:%M:%S %Z"

parse_time :: String -> String -> UTCTime
parse_time = readTime defaultTimeLocale
