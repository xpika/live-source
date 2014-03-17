{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

actions dpy win gc  = do 
   drawInWin 12 dpy win gc

liveMain = do
  putStrLn ""
  return actions

drawInWin x dpy win gc = do
 bgcolor <- initColor dpy "green"
 fgcolor <- initColor dpy "blue"
 setForeground dpy gc bgcolor
 fillRectangle dpy win gc 0 0 200 200
 setForeground dpy gc fgcolor
 let q = 96 * 0.3 
 fillRectangle dpy win gc ((2+fromIntegral x)) 2 (round $ q) (round q)
 
initColor :: Display -> String -> IO Pixel
initColor dpy color = do
 let colormap = defaultColormap dpy (defaultScreen dpy)
 (apros,real) <- allocNamedColor dpy colormap color
 return $ color_pixel apros
