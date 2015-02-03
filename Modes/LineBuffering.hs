module Net34.Modes.LineBuffering
	(connect
 	,disconnect
	,sync
	,async) where
import qualified Network	as N (HostName, PortID(PortNumber))
import qualified Net34.General.Net34 	as N
import qualified System.IO		as S.I
import qualified System.IO.Error	as S.I.E
import qualified Data.List		as D.L
import qualified Data.Maybe		as D.M
import qualified Control.Exception	as C.E 
import qualified Control.Monad		as C.M (guard, mapM_)
import qualified Control.Concurrent	as C (threadDelay)

wait :: Int -> IO ()
wait n = C.threadDelay (1000000*n)

connect :: N.HostName -> N.PortID -> Maybe S.I.NewlineMode -> IO S.I.Handle
connect	a p m	= do
	h <- N.connect a p S.I.LineBuffering 
	S.I.hSetNewlineMode h (maybe S.I.universalNewlineMode id m)
	return h

disconnect = N.disconnect

sync  :: S.I.Handle -> String -> IO [String]
sync  h q = do
	s <- N.sync h (robustPutStrLn 0 10) q ((readLinesWait 100))
	return $ maybe [] id s

async :: S.I.Handle -> String -> IO ()
async h q = N.async h (robustPutStrLn 0 10) q

robustPutStrLn :: Int -> Int -> S.I.Handle -> String -> IO ()
robustPutStrLn tries robust h s = do 
	r <- C.E.try (S.I.hPutStrLn h s)
	case r of
		Right e -> return ()
		Left  e ->
			if tries < robust && expectedEx e
			then do wait 1
				robustPutStrLn (1+tries) robust h s 
			else C.E.throw e
	where expectedEx e
		| S.I.E.isFullError e		= True
		| S.I.E.isPermissionError e	= True
		| otherwise			= False

readLinesWait :: Int -> S.I.Handle -> IO (Maybe [String])
readLinesWait n h = do
	r <- S.I.hWaitForInput h n -- ghc -threaded prevents blockng
	if   not r
	then return (Just [])
	else do g <- C.E.try (S.I.hGetLine h)
		case g of
	  		Left  e ->
				if   expected_ex e
		       		then return (Just [])
				else C.E.throw e
			Right s -> do
				r <- readLinesWait n h
				return $ Just (s : maybe [] id r)
	where expected_ex e
		| S.I.E.isEOFError e	= True
		| otherwise		= False
