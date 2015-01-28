module Net34.Modes.LineBuffering
	(connect
 	,disconnect
	,sync
	,async) where
import qualified Net34.General.Net34 as N
import qualified System.IO	 as S.I
import qualified Data.List 	 as D.L

connect		= N.connect
disconnect	= N.disconnect

sync  :: S.I.Handle -> String -> IO [String]
sync  h q = N.sync h S.I.hPutStrLn q readLinesEOF

async :: S.I.Handle -> String -> IO ()
async h q = N.async h S.I.hPutStrLn q

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
