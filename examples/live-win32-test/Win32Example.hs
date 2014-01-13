{-# OPTIONS_GHC -fno-cse #-} 

import qualified Graphics.Win32
import qualified System.Win32.DLL
import qualified System.Win32.Types
import Control.Exception (bracket,catch,SomeException)
import Foreign hiding (unsafePerformIO)
import System.Exit
import System.IO.Unsafe
import Data.IORef
import LiveSource
import Data.Time
import Control.Concurrent
import Data.Unsafe.Global 
import Control.Monad


main :: IO ()
main =
  Graphics.Win32.allocaPAINTSTRUCT $ \ lpps -> do
  hwnd <- createWindow 200 200 (wndProc lpps (onPaint))
  messagePump hwnd
  

{-
 OnPaint handler for a window - draw a string centred
 inside it.
-}
--onPaint :: Graphics.Win32.RECT -> Graphics.Win32.HDC -> IO ()
onPaint g (_,_,w,h) hdc = do
   Graphics.Win32.setBkMode hdc Graphics.Win32.tRANSPARENT
   Graphics.Win32.setTextColor hdc (Graphics.Win32.rgb 255 255 0)
   let y | h==10     = 0
         | otherwise = ((h-10) `div` 2)
       x | w==50     = 0
         | otherwise = (w-50) `div` 2
   maybeFunction <- loadAndRunFilePrintingErrorMessageUnsafeWithCache "live_win32_code.hs"
   case maybeFunction of 
     (Just function,cached) -> function cached g hdc x (y -3)
     _ -> return ()
   
   return ()
{-
wndProc :: Graphics.Win32.LPPAINTSTRUCT
	-> (Graphics.Win32.RECT -> Graphics.Win32.HDC -> IO ()) -- on paint action
        -> Graphics.Win32.HWND
        -> Graphics.Win32.WindowMessage
	-> Graphics.Win32.WPARAM
	-> Graphics.Win32.LPARAM
	-> IO Graphics.Win32.LRESULT
        -}
wndProc lpps onPaint hwnd wmsg wParam lParam
 | wmsg == Graphics.Win32.wM_DESTROY = do
     Graphics.Win32.sendMessage hwnd Graphics.Win32.wM_QUIT 1 0
     return 0
 | wmsg == Graphics.Win32.wM_SHOWWINDOW  = do
     
     Graphics.Win32.setWinTimer hwnd 1 10
     print "created timer"
     return 0
 | wmsg == Graphics.Win32.wM_TIMER = do
     changed <- getFileChangedUnsafe "live_win32_code.hs"
     when changed (Graphics.Win32.invalidateRect (Just hwnd) Nothing False)
     return 0
 | wmsg == Graphics.Win32.wM_PAINT && hwnd /= nullPtr = do
     r <- Graphics.Win32.getClientRect hwnd
     paintWith lpps hwnd (onPaint (Just hwnd) r)
    -- print "painted"
     return 0
 | otherwise = do
     Graphics.Win32.defWindowProc (Just hwnd) wmsg wParam lParam
     
  

createWindow :: Int -> Int -> Graphics.Win32.WindowClosure -> IO Graphics.Win32.HWND
createWindow width height wndProc = do
  let winClass = Graphics.Win32.mkClassName "Hello"
  icon         <- Graphics.Win32.loadIcon   Nothing Graphics.Win32.iDI_APPLICATION
  cursor       <- Graphics.Win32.loadCursor Nothing Graphics.Win32.iDC_ARROW
  bgBrush      <- Graphics.Win32.createSolidBrush (Graphics.Win32.rgb 0 0 255)
  mainInstance <- System.Win32.DLL.getModuleHandle Nothing
  Graphics.Win32.registerClass
  	  ( Graphics.Win32.cS_VREDRAW + Graphics.Win32.cS_HREDRAW
	  , mainInstance
	  , Just icon
	  , Just cursor
	  , Just bgBrush
	  , Nothing
	  , winClass
	  )
  w <- Graphics.Win32.createWindow
  		 winClass
		 "Hello, World example"
		 Graphics.Win32.wS_OVERLAPPEDWINDOW
		 Nothing Nothing -- leave it to the shell to decide the position
		 		 -- at where to put the window initially
                 (Just width)
		 (Just height)
		 Nothing      -- no parent, i.e, root window is the parent.
		 Nothing      -- no menu handle
		 mainInstance
		 wndProc
  Graphics.Win32.showWindow w Graphics.Win32.sW_SHOWNORMAL
  Graphics.Win32.updateWindow w
  return w

messagePump :: Graphics.Win32.HWND -> IO ()
messagePump hwnd = Graphics.Win32.allocaMessage $ \ msg ->
  let pump = do
        Graphics.Win32.getMessage msg (Just hwnd) 
		`catch` ((\ e -> exitWith ExitSuccess) ::  SomeException -> IO a)
	Graphics.Win32.translateMessage msg
	Graphics.Win32.dispatchMessage msg
	pump
  in pump

-- paintWith :: Graphics.Win32.LPPAINTSTRUCT -> Graphics.Win32.HWND -> (Graphics.Win32.HDC -> IO a) -> IO a
paintWith lpps hwnd p =
  bracket
    (Graphics.Win32.beginPaint hwnd lpps)
    (const $ Graphics.Win32.endPaint hwnd lpps  {-  >> Graphics.Win32.invalidateRect (Just hwnd) Nothing False -} )
    (p )
   