module Net34.General.Net34
	(connect
	,disconnect
	,sync
	,async) where
import qualified System.IO as S.I
import qualified Network   as N (HostName
			      , PortID(PortNumber)
			      , withSocketsDo
			      , connectTo)
import qualified Control.Exception   as E (throwIO)
import qualified Net34.General.Types as T
-- import qualified Control.Concurrent.Async as A (async, withAsync)
-- Exception handling should take place in those libraries
-- immediately above this abstraction. Therein, for the
-- function-arguments of sync, async.

connect :: N.HostName -> N.PortID -> S.I.BufferMode -> IO S.I.Handle
connect a p m = N.withSocketsDo $ do
	h <- N.connectTo a p
	S.I.hSetBuffering h m
	return h

disconnect :: S.I.Handle -> IO ()
disconnect = S.I.hClose

sync  :: S.I.Handle
	-> (S.I.Handle -> a -> IO b) -> a
	-> (S.I.Handle -> IO c)  -> IO c
sync  h f0 q f1 = send h f0 q >> recv h f1

recv :: S.I.Handle -> (S.I.Handle -> IO a) -> IO a
recv h f = do
	o <- S.I.hIsOpen h
	r <- S.I.hIsReadable h
	if and [o,r] then f h
	else E.throwIO T.RecvErr

-- A.withAsync (send h f0 q) ()
async :: S.I.Handle -> (S.I.Handle -> a -> IO b) -> a -> IO b
async h f0 q = send h f0 q

send  :: S.I.Handle -> (S.I.Handle -> a -> IO b) -> a -> IO b
send h f m = do
	o <- S.I.hIsOpen h
	w <- S.I.hIsWritable h
	if and [o,w] then f h m
	else E.throwIO T.SendErr
