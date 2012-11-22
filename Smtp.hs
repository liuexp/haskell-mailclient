module Smtp where
import Text.ParserCombinators.Parsec.Rfc2822 ( Message(..), GenericMessage(..), Field(..), NameAddr(..))
import Network.Socket
import Control.Exception	(bracketOnError,finally)
import System.IO
import Control.Monad		(forM_)
import Format

simpleMakeMessage title content from to ct=
		Message [From [NameAddr (Just from) from], To [NameAddr (Just to) to], Subject title, Date ct] content

sendMail :: (String -> IO()) -> String -> String -> String -> Message -> IO()
sendMail logger heloDomain smtpHost smtpPort message =
		getAddrInfo Nothing (Just smtpHost) (Just smtpPort) >>=
		(\smtpAddr -> sendSMTP logger heloDomain (head smtpAddr) message) 

sendSMTP :: (String -> IO()) -> String -> AddrInfo -> Message -> IO()
sendSMTP logger heloDomain smtpAddr message = 
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
				, combineMessages ["RCPT TO: " ++ to | to <- tos]
				, "DATA"
				, formatMessage message
				, "."
				, "QUIT"]
				$ \ line -> hPutStr h line >> hPutStr h "\r\n" >>logger line>>logger "\r\n"
			hFlush h
  			)`finally` hClose h
		)

