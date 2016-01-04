-- Main program

import Control.Exception
import SNTP.SNTP
import SNTP.Client
import Numeric(showFFloat)
import System.Environment (getArgs)

main = do putStrLn "SNTP client version 0.1"
	  args <- getArgs
	  case args of
	   [host] -> real host `catch` printException
	   _      -> putStrLn "usage: sntp server"

sf d = showFFloat (Just 4) d ""

real host = do putStrLn $ "querying host " ++ host
	       packet <- query host
	       putStrLn $ "delay: " ++ sf   (delay packet)
	       putStrLn $ "tdiff: " ++ sf   (tdiff packet)
	       putStrLn $ "trans: " ++ show (tsToClockTime (transTS packet))
	       setTime packet

printException :: SomeException -> IO ()
printException e = print e
