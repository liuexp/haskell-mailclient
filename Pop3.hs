module Pop3 where

import Network.Socket
import Control.Exception	(bracketOnError,finally)
import System.IO
import Control.Monad
import Data.List		(intercalate, isPrefixOf, isSuffixOf)
import Data.Char		(isSpace, isControl)
import Control.Applicative
import Data.Maybe

genAuthCommands u p = ["USER "++u, "PASS "++p]
genList = ["LIST"]
genList' id = ["LIST " ++ show id]
genRetr id= ["RETR "++ show id]
genQuit = ["QUIT"]

stripSpace = dropWhile (\x -> isSpace x || isControl x) 

getReply h doMultipleLine = do
		x <- hGetLine h
		if not doMultipleLine then 
			if "\r"  `isSuffixOf` x then return $ init x
			  else return x
		else do
			 rest <- readOther
			 return $ intercalate "\r\n"  (x:rest)
		where
			readOther = do
				line <- getReply h False
				if line == "." then return []
		   		else do
					others <- readOther
					return (line:others)

isMultipleLine:: String -> Bool
isMultipleLine x
	| equiv [x] genList	= True
	| equiv [x] (genRetr 0)	= True
	|otherwise = False
	where
		unpackC = words . head 
		equiv a b = length ua == length ub && head ua == head ub where
			ua = unpackC a
			ub = unpackC b
		

checkReply :: (String -> IO()) -> String -> IO (Bool, String)
checkReply log contents 
	| isPrefixOf "+OK" $head strippedMsg = log logMsg >> return (True, logMsg)
	| otherwise = log logMsg >> return (False, logMsg)
	where strippedMsg = filter (not . null) $ map stripSpace $ lines contents
       	      logMsg = intercalate "\r\n" strippedMsg

listMail log popAddr user pass = 
		let cmdList = genAuthCommands user pass ++ genList  ++ genQuit in
			talk log cmdList popAddr genList

listMail' log popAddr user pass id = 
		let cmdList = genAuthCommands user pass ++ genList' id  ++ genQuit in
			talk log cmdList popAddr (genList' id)

retrMail log popAddr user pass id =
		let cmdList = genAuthCommands user pass ++ genRetr id ++ genQuit in
			talk log cmdList popAddr $genRetr id

talk log cmdList popAddr msgCmd= bracketOnError (socket (addrFamily popAddr) Stream defaultProtocol)
				sClose
				(\sock -> do
    					connect sock (addrAddress popAddr)
					socketToHandle sock ReadWriteMode
				) >>= ( \ h -> do 
	  				--hSetBuffering h LineBuffering 
					getReply h False
	  				msg <- mapM (\cmd ->
	  					hPutStr h cmd >>hPutStr h "\r\n" >> hFlush h>>log cmd >> log "\r\n" >>getReply h (isMultipleLine cmd) >>= checkReply log >>= (\(res, msg)->
										if res then return (cmd, msg) else fail $ "talking failed in " ++ cmd ++ "\n" ++ msg
										)) 
						cmdList -- >>=
					--msgRest <- hGetLine h
					hClose h
					return . fromMaybe "" . lookup (head msgCmd) $msg
					) 
