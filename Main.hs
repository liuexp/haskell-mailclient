--- Here's a simple test that the above function does work.
module Main where
import System.Environment
import System.Time
    ( CalendarTime(..)
    , getClockTime
    , toCalendarTime
    )
import Smtp
import System.IO
main = do
	 ct <- toCalendarTime =<< getClockTime
	 let msg = simpleMakeMessage "hello" "test" "liuexp" "testmail" ct in
		 sendMail (hPutStrLn stderr) "liuexp" "59.78.44.239" "25" msg

