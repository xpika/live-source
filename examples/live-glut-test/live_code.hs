import Graphics.UI.GLUT
import System.IO.Unsafe
import Data.IORef

newGlobal = unsafePerformIO $ newIORef 0

axi = (Vector3 0 1 (0::GLfloat))



-- function :: IO ()
function state = do
    g <- readIORef newGlobal
    modifyIORef newGlobal (+0.3)
    rotate g axi

liveMain = do
  putStrLn ""
  return (function)