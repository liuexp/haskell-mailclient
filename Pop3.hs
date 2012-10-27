module Pop3 where

import Network.Socket
import Control.Exception	(bracketOnError,finally)
import System.IO
import Data.List		(intercalate, isPrefixOf, isSuffixOf)
import Data.Char		(isSpace, isControl)
import Data.Maybe

genAuth u p = ["USER "++u, "PASS "++p]
genList = ["LIST"]
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
	|unpackC [x] == unpackC genList = True
	|unpackC [x] == unpackC (genRetr 0) = True
	|otherwise = False
	where
		unpackC = head . words . head 

checkReply contents 
	| isPrefixOf "+OK" $head strippedMsg =  (True, logMsg)
	| otherwise = (False, logMsg)
	where strippedMsg = filter (not . null) $ map stripSpace $ lines contents
       	      logMsg = intercalate "\r\n" strippedMsg

listMail = talk genList
retrMail id = talk (genRetr id)

talk cmdList log popAddr user pass = bracketOnError (socket (addrFamily popAddr) Stream defaultProtocol)
				sClose
				(\sock -> do
    					connect sock (addrAddress popAddr)
					socketToHandle sock ReadWriteMode
				) >>= ( \ h -> (do 
	  				--hSetBuffering h LineBuffering 
					getReply h False
	  				msg <- mapM (\cmd ->
	  					hPutStr h cmd >>
						hPutStr h "\r\n" >> 
						hFlush h >>
						log cmd >> 
						log "\r\n" >>
						getReply h (isMultipleLine cmd) >>= 
						return . checkReply >>= 
						(\(res, msg)->
								if res then return (cmd, msg) else fail $ "talking failed in " ++ cmd
						 )) 
						(genAuth user pass ++ cmdList ++ genQuit) 
					return . fromMaybe "" . lookup (head cmdList) $msg
					) `finally` hClose h
				)

