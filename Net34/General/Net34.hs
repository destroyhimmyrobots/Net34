module Net34.General.Net34
	(connect
 	,connectUnsafe
	,disconnect
	,sync
	,async) where
import qualified System.IO	 as S.I
import qualified Data.List 	 as D.L
import qualified Network	 as N (HostName, PortID(PortNumber), withSocketsDo, connectTo)
import qualified Data.Word	 as D.W
import qualified System.IO.Unsafe as S.I.U

connect :: N.HostName -> N.PortID -> Maybe S.I.BufferMode -> IO S.I.Handle
connect a p m = N.withSocketsDo $ do
	h <- N.connectTo a p
	S.I.hSetBuffering h (maybe S.I.LineBuffering id m)
	return h

connectUnsafe :: N.HostName -> N.PortID -> Maybe S.I.BufferMode -> S.I.Handle
connectUnsafe a p m = S.I.U.unsafePerformIO $ connect a p m

disconnect :: S.I.Handle -> IO ()
disconnect = S.I.hClose

sync  :: S.I.Handle
	-> (S.I.Handle -> a -> IO ()) -> a
	-> (S.I.Handle -> IO b)  -> IO b
sync  h f0 q f1 = send h f0 q >> recv h f1

async :: S.I.Handle -> (S.I.Handle -> a -> IO ()) -> a -> IO ()
async h f q = send h f q

send :: S.I.Handle -> (S.I.Handle -> a -> IO ()) -> a -> IO ()
send h f m = do
	o <- S.I.hIsOpen h
	w <- S.I.hIsWritable h
	if and [o,w] then f h m
	else return ()
		
e :: String -> IO a
e = ioError . userError

recv :: S.I.Handle -> (S.I.Handle -> IO a) -> IO a
recv h f = do
	o <- S.I.hIsOpen h
	r <- S.I.hIsReadable h
	if and [o,r] then f h
	else e "Handle unopen or unreadable."

readLinesEOF :: S.I.Handle -> IO [String]
readLinesEOF h = do -- Must use the -threaded flag to prevent blocking
	r <- S.I.hWaitForInput h 2500
	if r then do 
		e <- S.I.hIsEOF h
		if e then return []
		else do n <- S.I.hGetLine h
			r <- readLinesEOF h
			return $ n : r
	else return []
