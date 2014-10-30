{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.SVG

sierpinski 1 = square 1
sierpinski n = ( s ||| square n ||| s)  # centerX
                  ===
               (s ||| square n |||  s) # centerX
  where s = sierpinski (n-1)

example = sierpinski 7  # centerXY # lw 0 # fc yellow
                   `atop` square 500 # fc blue
                   
liveMain = do
           print 122
           renderSVG "file.svg" (Width 1000) (example :: Diagram B R2)    