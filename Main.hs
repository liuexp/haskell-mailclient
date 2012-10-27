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
port = "110"

main = do
			addr <- getAddrInfo Nothing (Just host) (Just port) 
			putStrLn "---- Here's the list debug -----"
			msg <- listMail (hPutStrLn stderr) (head addr) user pass
			putStrLn "---- Here's the list result -----"
			putStrLn msg
			putStrLn "---- Here's the retr debug -----"
			msg <- retrMail (hPutStrLn stderr) (head addr) user pass 19
			putStrLn "---- Here's the retr result -----"
			putStrLn msg
{--
main = do
	 ct <- toCalendarTime =<< getClockTime
	 let msg = simpleMakeMessage "hello" "test" "liuexp" "testmail" ct in
		 --sendMail (hPutStrLn stderr) "liuexp" "59.78.44.239" "25" msg
		 sendMail (hPutStrLn stderr) "liuexp" "127.0.0.1" "25" msg

--}
