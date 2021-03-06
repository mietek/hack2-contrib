-- | Stolen from rack: catches all exceptions raised from the app it wraps.

module Hack2.Contrib.Middleware.ShowExceptions (show_exceptions) where


import Data.Default
import Data.Maybe
import Hack2
import Hack2.Contrib.Middleware.Hub
import Hack2.Contrib.Utils
import Air.Light
import Prelude hiding ((.), (^), (>), (-), log)
import System.IO  
import System.IO.Error
import qualified Data.ByteString.Char8 as B

program :: String
program = "ShowExceptions"

show_exceptions :: Maybe HackErrors -> Middleware
show_exceptions stream app = \env -> do
  let my_stream = unHackErrors - stream.fromMaybe (env.hack_errors)
  let log = simple_logger (\x -> my_stream (B.pack x)) program
  
  app env `catch` (handler log)

  where
    handler :: Logger -> IOError -> IO Response
    handler log e = do
      let message = e.show
      Error. log message
      return - def { status = 500, body = B.pack message }
