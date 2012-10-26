module Main where
import Text.ParserCombinators.Parsec.Rfc2821 (
        SmtpReply(..),
        SmtpCode(..),
        SuccessCode(..),
        Category(..),
        reply
    )
import Text.ParserCombinators.Parsec.Rfc2822 (
        Message(..),
        GenericMessage(..),
        Field(..),
        NameAddr(..)
    )
import System.Time
    ( CalendarTime(..)
    , getClockTime
    , toCalendarTime
    )
import Data.IORef (newIORef, readIORef)
import Network.Socket
import Control.Exception	(bracketOnError,finally)
import System.IO
import Control.Monad		(forM_)
import System.Environment


angleAddr :: String -> ShowS
angleAddr addr = (addr ++)

sendMail :: (String -> IO()) -> String -> String -> String -> Message -> IO()
sendMail log heloDomain smtpHost smtpPort message =
		getAddrInfo Nothing (Just smtpHost) (Just smtpPort) >>=
		(\smtpAddr -> sendSMTP log heloDomain (head smtpAddr) message) 
		-- TODO : error handling
--		(\status -> case head status of
--					       Nothing -> return ()
--					       Just x -> log $ "SMTP send failed" ++ show x)

sendSMTP :: (String -> IO()) -> String -> AddrInfo -> Message -> IO()
sendSMTP log heloDomain smtpAddr message = 
		bracketOnError (socket (addrFamily smtpAddr) Stream defaultProtocol) 
			sClose
			(\sock -> do
   				connect sock (addrAddress smtpAddr)
				socketToHandle sock ReadWriteMode
			) >>= (\ h -> 
			(do
  			let Message fields _ = message
			    froms = map (\(NameAddr _ addr) -> addr) $
				            concatMap (\f -> case f of
				                    From from -> from
				                    _         -> []) fields

			    tos = map (\(NameAddr _ addr) -> addr) $
		                    concatMap (\f ->
		                        case f of
		                            To to  -> to
		                            Cc to  -> to
		                            Bcc to -> to
		                            _      -> []) fields in


			    forM_ [ "EHLO " ++ heloDomain 
	   			,"MAIL FROM: " ++ angleAddr (head froms) "" 
				,"RCPT TO: " ++ angleAddr (head tos) "" 
				, "DATA"
				-- TODO: format message
				, "."
				, "QUIT"]
				$ \ line -> hPutStr h line >> hPutStr h "\r\n" >>log line>>log "\r\n"
			hFlush h
  			)`finally` hClose h
		)

--- Here's a simple test that the above function does work.
{--
simpleMakeMessage title content from to ct=
		Message [From [NameAddr (Just from) from], To [NameAddr (Just to) to], Subject title, Date ct] content

main = do
	 ct <- toCalendarTime =<< getClockTime
	 let msg = simpleMakeMessage "hello" "test" "liuexp" "testmail" ct in
		 sendMail (hPutStrLn stderr) "liuexp" "59.78.44.239" "25" msg

--}
