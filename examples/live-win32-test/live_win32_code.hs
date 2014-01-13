import qualified Graphics.Win32
import qualified System.Win32.DLL
import qualified System.Win32.Types
import qualified Graphics.Win32.GDI.Font
import Control.Concurrent
import System.IO.Unsafe
import Control.Monad
--import Data.Unsafe.Global
import Data.IORef
import Data.Time

resources = unsafePerformIO $ newIORef Nothing

newFunc cached hwnd hdc x y = do
    resources' <- readIORef resources
    
    (font,b) <- case resources' of 
	Nothing -> do
	    font <- Graphics.Win32.createFont 
		20
		0
		0
		0
		Graphics.Win32.fW_BOLD 
		False False False Graphics.Win32.aNSI_CHARSET  
		Graphics.Win32.oUT_DEFAULT_PRECIS
		Graphics.Win32.cLIP_DEFAULT_PRECIS 
		Graphics.Win32.dEFAULT_QUALITY 
		Graphics.Win32.dEFAULT_PITCH
		"Courier New"
	    b <- Graphics.Win32.createSolidBrush (Graphics.Win32.rgb 0 0 255)
	    let resources'' = (font,b)
	    writeIORef resources (Just resources'')
	    return resources''
	Just x -> return x
    Graphics.Win32.fillRect hdc (0,0,500,500) b  
    Graphics.Win32.selectFont hdc font
    mapM_ (\(line,num) -> Graphics.Win32.textOut hdc (x-20) (y+(15*num)-20) line )
        (zip ( ["hello" , "world"] ) [1..] )

liveMain = do
	   x <- putStr ""
	   return newFunc