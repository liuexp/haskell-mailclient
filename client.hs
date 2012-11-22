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
    		(\cmd -> case cmd of
      (Send {from=from, to=to, subject=subject, message=message}) -> do
	      ct <- toCalendarTime =<< getClockTime
	      rv <- runErrorT $
	      		do
				cf <- join $ liftIO $ readfile emptyCP "config.ini"
				host <- get cf "smtp" "host"
				port <- get cf "smtp" "port"
				let msg = simpleMakeMessage subject message from to ct
				liftIO $ sendMail (hPutStrLn stderr) to host port msg
				return "done"
	      putStrLn $ formatEither rv
      (List _) -> do 
		     rv <- runErrorT $
		      do
			      cf <- join $ liftIO $ readfile emptyCP "config.ini"
			      host <- get cf "pop" "host"
			      port <- get cf "pop" "port"
			      user <- get cf "pop" "user"
			      pass <- get cf "pop" "pass"
			      addr <- liftIO $ getAddrInfo Nothing (Just host) (Just port)
			      msg <- liftIO $ listMail (hPutStrLn stderr) (head addr) user pass
			      liftIO $ putStrLn msg
			      return msg
		     putStrLn $ formatEither  rv
		

      (Retr {num=num}) -> do 
			     rv <- runErrorT $
			     	do
					cf <- join $ liftIO $ readfile emptyCP "config.ini"
					host <- get cf "pop" "host"
					port <- get cf "pop" "port"
					user <- get cf "pop" "user"
					pass <- get cf "pop" "pass"
					addr <- liftIO $ getAddrInfo Nothing (Just host) (Just port) 
					msg <- liftIO $ retrMail (hPutStrLn stderr) (head addr) user pass (read num ::Int)
					liftIO $ putStrLn msg
					return msg
			     putStrLn $ formatEither rv
      )

