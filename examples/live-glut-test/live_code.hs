import Graphics.UI.GLUT
import System.IO.Unsafe
import Data.IORef
import Data.List
import Control.Monad
import Data.Ord

newGlobal = unsafePerformIO $ newIORef 0

axi = (Vector3 0 1 (1::GLfloat))

-- function :: IO ()
function state = do
    g <- readIORef newGlobal
    modifyIORef newGlobal (+0.4)
    rotate g axi
    renderPrimitive Polygon $ 
     -- mapM (\[x,y] ->easy_vert x y 0)
      --( map (map (/2))
       --(sortBy  
           --(comparing (\[x,y] -> atan (x/y)))
         --  (liftM2 (\x y -> [x,y]) [0.15,0.3,0.2,0.4,0.1,0.5] [0.9,2,0.32,0.8,0.7,0.5]) 
       -- )
     -- )
      sequence $ interleave rainbow grid

liveMain = do
  putStrLn ""
  return (function)

easy_vert x y z = vertex (Vertex3 (x::GLfloat) y z)

interleave =  (concat .) . zipWith (\x y-> [x,y]) 
rainbow  =  cycle $ liftM3 to_color colors colors colors
to_color x y z = do currentColor $= Color4 x y z 1
colors   =  [0,0.25..1]
grid = do (x,y,z) <- liftM3 (,,) list list list
          return (vertex (Vertex3 (x::GLfloat) y z))

list = [-0.5,-0.45..0.5]
