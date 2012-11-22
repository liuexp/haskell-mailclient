{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}
import System.Console.CmdArgs.FromHelp
import System.Console.CmdArgs hiding (cmdArgsHelp)
import System.Environment (getArgs)
import System.Time
    ( CalendarTime(..)
    , getClockTime
    , toCalendarTime
    )
import Smtp
import Pop3
import Network.Socket(getAddrInfo)
import Control.Monad.Error
import Data.ConfigFile
import Format
import System.IO


mkCmdArgs [fromHelp|
  Mail client v0.1 in Haskell by Liuexp.


client [COMMAND] ... [OPTIONS]

Common flags
  -? --help       Display help message
  -V --version    Print version information

client send [OPTIONS]

  -f --from	Sender/From field.
  -t --to	Receiver/To field.
  -s --subject  Subject/Title of your message.
  -m --message  The message body.


client retr [OPTIONS]

  -n --num	Message ID to be retrieved.

client list [OPTIONS]

  -n --num	Message ID to be listed, if not specified, all mails will be listed.
|]


main :: IO ()
main = do
  args <- getArgs
  if "--help" `elem` args || "-?" `elem` args || null args
    then putStrLn clientHelpContents
    else cmdArgs (modes [defaultSend, defaultList, defaultRetr]) >>= 
    		(\cmd -> do 
      			 rv <- runErrorT $
			 		do
						cf <- join $ liftIO $ readfile emptyCP "config.ini"
						smtpHost <- get cf "smtp" "host"
						smtpPort <- get cf "smtp" "port"
						popHost <- get cf "pop" "host"
						popPort <- get cf "pop" "port"
						popUser <- get cf "pop" "user"
						popPass <- get cf "pop" "pass"
						hLogfile <- liftIO $ openFile "client.log" AppendMode
						let logger = hPutStrLn hLogfile
	  					liftIO $ logger "----- New log segment start-------"
						case cmd of
	      						(Send {from=from, to=to, subject=subject, message=message}) -> do
								ct <- liftIO $ toCalendarTime =<< getClockTime
								let msg = simpleMakeMessage subject message from to ct
	    							liftIO $ sendMail logger to smtpHost smtpPort msg
								return ""
							(List {num=num}) -> do
								addr <- liftIO $ getAddrInfo Nothing (Just popHost) (Just popPort)
								msg <- if null num then liftIO $ listMail logger (head addr) popUser popPass
			   						 else liftIO $ listMail' logger (head addr) popUser popPass (read num :: Int)
								liftIO $ putStrLn msg
								return msg
							(Retr {num=num}) -> do
								addr <- liftIO $ getAddrInfo Nothing (Just popHost) (Just popPort)
								msg <- liftIO $ retrMail logger (head addr) popUser popPass (read num :: Int)
								liftIO $ putStrLn msg
								return msg
							_ -> error "something wrong with CLI parsing"
						liftIO $ hFlush hLogfile
						liftIO $ hClose hLogfile
      			 hPutStrLn stderr $ formatEither rv
      )

