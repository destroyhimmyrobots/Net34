{-# LANGUAGE DeriveDataTypeable #-}
module Net34.General.IPCException
	(e
	,isEHandleClosed	
	,isEHandleUnopen
	,isEHandleUnreadable 
	,isEHandleUnwritable 
	,isESend
	,isERecv
 	,IPCException(..)) where
import qualified Data.Typeable	 as DT
import qualified Control.Exception as E 

data IPCException
	= ESend
	| ERecv
	| EHandleUnopen
	| EHandleClosed
	| EHandleUnreadable
	| EHandleUnwritable
	deriving (Enum, Eq, Show, DT.Typeable)
instance E.Exception IPCException

e :: String -> IO a
e = ioError . userError

isESend :: IPCException -> Bool
isERecv :: IPCException -> Bool
isESend 	ESend	= True
isESend 	_	= False
isERecv 	ERecv	= True
isERecv 	_	= False
isEHandleClosed		:: IPCException -> Bool
isEHandleUnreadable 	:: IPCException -> Bool
isEHandleUnwritable 	:: IPCException -> Bool
isEHandleClosed		EHandleClosed		= True
isEHandleClosed		_			= False
isEHandleUnopen		EHandleUnopen		= True
isEHandleUnopen		_			= False
isEHandleUnreadable 	EHandleUnreadable	= True
isEHandleUnreadable 	_			= False
isEHandleUnwritable 	EHandleUnwritable	= True
isEHandleUnwritable 	_			= False
