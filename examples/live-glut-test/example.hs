
import Graphics.UI.GLUT
import Control.Monad.Cont
import Data.IORef(IORef)
import qualified Data.IORef as IORef
import qualified Foreign.C.Types
import LiveSource

newIORef x = lift (IORef.newIORef x)
writeIORef x y = lift (IORef.writeIORef x y)
readIORef x = lift (IORef.readIORef x)
modifyIORef x y = lift (IORef.modifyIORef x y)

type GLFloat = Foreign.C.Types.CFloat

type ContIO r a = ContT r IO a
defines :: ContIO r (IORef.IORef (() -> ContIO GLFloat GLFloat), ContIO GLFloat GLFloat)
defines = do
   contref <- newIORef undefined
   let test = do
           i <- newIORef 0
           callCC (\k -> writeIORef contref k)
           modifyIORef i (+ 1)
           readIORef i
   return (contref, test)
  

run :: ContIO a a -> IO a
run m = runContT m return

main = do
   (contref, test) <- run defines
   run test >>= print
   getArgsAndInitialize
   createAWindow "points"
   mainLoop
  
createAWindow windowName = do
  (contref, test) <- run defines
  run test
  initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer,
                             WithAlphaComponent ]
  createWindow windowName
  displayCallback $= display contref
  idleCallback $= Just (display contref)

axis = (Vector3 1 1 (1::GLfloat))


list = [-0.5,-0.45..0.5]
display thingy = do 
  clear [ColorBuffer , DepthBuffer]
  loadIdentity
  x1 <- run (readIORef thingy >>= \k -> k())
  maybeFunction <- loadAndRunFilePrintingErrorMessageUnsafeWithCache "live_code.hs"
   
  case maybeFunction of 
    (Just function,cached) -> function Nothing
    _ -> return ()
  {-
  renderPrimitive Points $ 
    sequence $ interleave rainbow grid
  -}
  swapBuffers
  flush

{-
interleave =  (concat .) . zipWith (\x y-> [x,y]) 
rainbow  =  cycle $ liftM3 to_color colors colors colors
to_color x y z = do currentColor $= Color4 x y z 1
colors   =  [0,0.25..1]
grid = do (x,y,z) <- liftM3 (,,) list list list
          return (vertex (Vertex3 (x::GLfloat) y z))
-}
