import Graphics.UI.GLUT
import System.IO.Unsafe
import Data.IORef
import Data.List
import Control.Monad
import Data.Ord
import Data.Complex

newGlobal = unsafePerformIO $ newIORef 0

axi = (Vector3 0 1 (1::GLfloat))

-- function :: IO ()
function state = do
    g <- readIORef newGlobal
    modifyIORef newGlobal (+0.4)
    rotate g axi
    sequence [
     renderPrimitive Lines $ mapM (\(x:+y) -> easy_vert (insertAt q z [x,y])) ( (concat $ take 4 $ iterate (map (*(0:+1))) [(-0.5):+(-0.5),(-0.5):+0.5])) | z <- [-0.5,0.5], q <-[0..2] ]
      
insertAt = (\n x xs -> case splitAt n xs of { (a, b) -> a ++ [x] ++ b })

liveMain = do
  putStrLn ""
  return (function)

easy_vert [x,y,z] = vertex (Vertex3 (x::GLfloat) y z)

interleave =  (concat .) . zipWith (\x y-> [x,y]) 
rainbow  =  cycle $ liftM3 to_color colors colors colors
to_color x y z = do currentColor $= Color4 x y z 1
colors   =  [0,0.25..1]
grid = do (x,y,z) <- liftM3 (,,) list list list
          return (vertex (Vertex3 (x::GLfloat) y z))

list = [-0.5,-0.45..0.5]
