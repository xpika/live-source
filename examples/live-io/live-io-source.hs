import Control.Monad.Reader


{-# LANGUAGE NoMonomorphismRestriction #-}

inc = (+1)
dec = (subtract 1)

add 0 b = b
add a b = add (dec a) (inc b)

multiply a 0 = 0
multiply a b = add a (multiply a (dec b))

power a 0 = 1
power a b = multiply a (power a (dec b))


liveMain =  print $ power 5 5