module HSNTP.Util.Daemon (daemonize, childLives) where

import System.Posix

daemonize :: IO () -> IO () -> IO ProcessID
daemonize hup comp = forkProcess cont
    where cont = do createSession
		    installHandler lostConnection (Catch hup) Nothing
		    comp


childLives :: ProcessID -> IO Bool
childLives pid = do sleep 1
		    ms <- getProcessStatus False False pid
		    return $ maybe True (const False) ms
