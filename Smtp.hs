module Smtp where
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
import Data.IORef (newIORef, readIORef)
import Network.Socket
import Control.Exception	(bracketOnError,finally)
import System.IO
import Control.Monad		(forM_)
import Data.List		(intercalate)
import System.Time
import System.Locale

simpleMakeMessage title content from to ct=
		Message [From [NameAddr (Just from) from], To [NameAddr (Just to) to], Subject title, Date ct] content

combineMessages = intercalate "\r\n"

formatName xx@(NameAddr mName addr)=(case mName of
						Just y -> show y
						Nothing -> ""
						) ++ angleAddr addr ""
formatNameList x =intercalate ",\n\t" $ map formatName x
formatFields (Sender x) = "From: " ++ formatName x
formatFields (From x) = "From: " ++ formatNameList x
formatFields (Subject x) = "Subject: " ++ x
formatFields (To x) = "To: " ++ formatNameList x
formatFields (Bcc x) = "Bcc: " ++ formatNameList x
formatFields (Cc x) = "Cc: " ++ formatNameList x
formatFields (Date x) = "Date: " ++ formatCalendarTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S " x ++ ctTZName x
formatFields _ = ""

formatMessage :: Message -> String
formatMessage message@(Message fields body) = combineMessages $ escapeDot (fFields ++ [""] ++ fBody) where
							fBody = map (reverse . dropWhile (=='\r') . reverse ) $ lines body
	    						fFields = concatMap (lines. formatFields) fields
							escapeDot = map (\x -> case x of
										"." -> ". "
										otherwise-> x) 



angleAddr :: String -> ShowS
angleAddr addr = (("<"++addr++">") ++)

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
				-- FIXME: any better way to do this?
				, combineMessages ["RCPT TO: " ++ to | to <- tos]
				, "DATA"
				, formatMessage message
				, "."
				, "QUIT"]
				$ \ line -> hPutStr h line >> hPutStr h "\r\n" >>log line>>log "\r\n"
			hFlush h
  			)`finally` hClose h
		)

