import SNTP.SNTP
import HSNTP.Util.UDPServer
import HSNTP.Util.Daemon

import Control.Monad (when)

server = stdUDPServer { putFun = \a b -> putPacket a b >>= return . fst,
			getFun = parsePacket,
			workFun= reply,
			port   = 123
		      }

reply :: Packet -> IO Packet
reply p =
    do ref <- getCurrentTimeStamp
       return $ Packet (liVerMode 0 1 4) -- accurate, version 1, reply
		       13                -- we are a secondary timesource
		       7                 -- you can poll me every 128 seconds
		       15                -- precision
		       0 0               -- lie about the root
		       "LOCL"            -- uncalibrated local clock
		       ref               -- reference timestamp
		       (transTS p)       -- originate <- transmit
		       (received p)      -- recv      <- receive time
		       nilTS             -- Transmit gets written in putPacket
		       Nothing           -- No authentication
		       nilTS             -- Not a received packet


main = do putStrLn  "SNTP server v. 0.1"
	  pid <- daemonize (return ()) (runUDPServer server >> waitForever)
	  e   <- childLives pid
	  when e $ putStrLn ("pid = " ++ show pid)
