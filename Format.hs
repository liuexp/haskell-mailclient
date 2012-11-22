module Format where
import Text.ParserCombinators.Parsec.Rfc2822 (
        Message(..),
        GenericMessage(..),
        Field(..),
        NameAddr(..)
    )
import Data.List		(intercalate)
import System.Time
import System.Locale

------------General Format------------
formatEither :: (Show e0) => Either e0 a0 -> String
formatEither x = case x of
			 (Left y) -> "Error: " ++ show y
			 (Right _) -> "Succeeded!"


---------------SMTP Part--------------
angleAddr :: String -> ShowS
angleAddr addr = (("<"++addr++">") ++)

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


