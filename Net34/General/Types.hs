{-# LANGUAGE DeriveDataTypeable #-}
module Net34.General.Types
	(e
 	,isSendErr
	,isRecvErr
 	,IPCException(..)) where
import qualified Data.Typeable	 as DT
import qualified Control.Exception as E 

data IPCException
	= SendErr
	| RecvErr
	deriving (Enum, Eq, Show, DT.Typeable)
instance E.Exception IPCException

e :: String -> IO a
e = ioError . userError

isSendErr :: IPCException -> Bool
isRecvErr :: IPCException -> Bool
isSendErr SendErr	= True
isSendErr _		= False
isRecvErr RecvErr	= True
isRecvErr _		= False
