module Net34.General.Net34
	(connect
	,disconnect
	,sync
	,async) where
import qualified System.IO	 as S.I
import qualified Data.List 	 as D.L
import qualified Network	 as N (HostName, PortID(PortNumber), withSocketsDo, connectTo)
import qualified Data.Word	 as D.W
import qualified System.IO.Error as S.I.E
import qualified Control.Exception as C.E
import qualified Net34.General.IPCException as E

connect :: N.HostName -> N.PortID -> S.I.BufferMode -> IO S.I.Handle
connect a p m = N.withSocketsDo $ do
	h <- N.connectTo a p
	S.I.hSetBuffering h m
	return h

disconnect :: S.I.Handle -> IO ()
disconnect = S.I.hClose

sync  :: S.I.Handle
	-> (S.I.Handle -> a -> IO ()) -> a
	-> (S.I.Handle -> IO (Maybe b))
	-> IO (Maybe b)
sync  h f0 q f1 = send h f0 q >> recv h f1
-- ^ Possibly use applicative functors

async :: S.I.Handle -> (S.I.Handle -> a -> IO ()) -> a -> IO ()
async h f q = send h f q

send :: S.I.Handle -> (S.I.Handle -> a -> IO ()) -> a -> IO ()
send h f m = C.E.catchJust
	(\e -> send_ex e)
	(send_ h f m)
	(\e -> do S.I.hPutStrLn S.I.stderr $ "\tsend: handle:\t"    ++ show h
		  S.I.hPutStrLn S.I.stderr $ "\tsend: exception:\t" ++ show e)
	where	send_ h f m = do
			o <- S.I.hIsOpen h
			if   not o then C.E.throwIO $ S.I.E.userError "not open."
			else do r <- S.I.hIsWritable h
				if   not r
	   			then C.E.throwIO    $ S.I.E.userError "not writable."
				else f h m
		send_ex e -- In the future, make a robust handle wrapper.
			| S.I.E.isDoesNotExistErrorType t = Just e
			| S.I.E.isUserErrorType t = Just e
			| otherwise = Nothing
			where t = S.I.E.ioeGetErrorType e

recv :: S.I.Handle -> (S.I.Handle -> IO (Maybe a)) -> IO (Maybe a)
recv h f = C.E.catchJust
	(\e -> recv_ex e)
	(recv_ h f)
	(\e -> do S.I.hPutStrLn S.I.stderr $ "\tsend: handle:\t"    ++ show h
		  S.I.hPutStrLn S.I.stderr $ "\tsend: exception:\t" ++ show e
		  return Nothing)
	where	recv_ :: S.I.Handle -> (S.I.Handle -> IO (Maybe a)) -> IO (Maybe a)
		recv_ h f = do
			o <- S.I.hIsOpen h
			if   not o then C.E.throwIO $ S.I.E.userError "not open."
			else do r <- S.I.hIsReadable h
				if   not r
	   			then C.E.throwIO    $ S.I.E.userError "not readable."
				else f h
		recv_ex e -- In the future, make a robust handle wrapper.
			| S.I.E.isDoesNotExistErrorType t = Just e
			| S.I.E.isUserErrorType t = Just e
			| otherwise = Nothing
			where t = S.I.E.ioeGetErrorType e
			-- | E.isEHandleUnopen e		= Just $ show e
			-- | E.isEHandleUnreadable e	= Just $ show e
