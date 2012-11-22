--- Here's a simple test that the above function does work.
module Main where
import System.Environment
import System.Time
    ( CalendarTime(..)
    , getClockTime
    , toCalendarTime
    )
import Smtp
import Pop3
import System.IO
import Network.Socket

user = "testmail"
pass = "testmail"
host = "liuexp"
popPort = "110"
smtpPort = "25"

main = do
		addr <- getAddrInfo Nothing (Just host) (Just popPort) 
		putStrLn "---- Here's the list debug -----"
		msg <- listMail (hPutStrLn stderr) (head addr) user pass
		putStrLn "---- Here's the list result -----"
		putStrLn msg
		putStrLn "---- Here's the retr debug -----"
		msg2 <- retrMail 25 (hPutStrLn stderr) (head addr) user pass 
		putStrLn "---- Here's the retr result -----"
		putStrLn msg2
		putStrLn "---- Here's the smtp debug -----"
	 	ct <- toCalendarTime =<< getClockTime
		addr2 <- getAddrInfo Nothing (Just host) (Just smtpPort) 
		let msg = simpleMakeMessage "hello" "test" "liuexp" "testmail" ct in
			sendMail (hPutStrLn stderr) host (head addr2) msg

