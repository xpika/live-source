import Data.Numbers.Primes
import System.Cmd
import Data.List
import Control.Monad
import System.IO

points = [ 
            "   "
           ,"###"
           ,"   "
          ]

chunks n [] = []
chunks n xs = let firstChunk = take n xs in firstChunk : chunks n (drop n xs)

initialState = map (\[x,y]->[x+20,y]) initialState''
initialState'' = [             [2,0]
                 ,[0,1],[1,1]
                       ,[1,2],[2,2]
               ]

initialState' = [[0,1],[1,1],[2,1]]

initialState2 = [[1,0],[1,1],[1,2]]

makeTextCanvas d = unlines $ replicate d $ replicate d '.'
makeGrid d = replicateM 2 [0..d] 

--findNeighbours (x,y) =  

--gridWindow p i ((x:xs):xss) = 
--

textCooridinateToUnary text (x,y) = gridCoordinateToUnary (map (undefined:) $ lines text) (x,y)
replaceAt i v xs = map (\(x,i') -> if i==i' then v else x) $  zip xs [0..]

replaceAtText v (x,y) text = replaceAt (textCooridinateToUnary text (x,y)) v text

gridCoordinateToUnary grid (x,y) = coordinateToUnary (length (head grid)) (x,y)
coordinateToUnary width (x,y) = (width * y) + x

moves = replicateM 2 [-1..1]
neighboringMoves = replicateM 2 [(-1)..1] \\ [[0,0]]
neighboringAreas c@[x,y] = map (zipWith (+) c) moves
findNeighbors cs c = neighboringAreas c `intersect` cs

countNeighbors  = ((length .) . findNeighbors) 

applyAll v fs = foldr ($) v (reverse fs)

meetsAll =  ((foldl1 (&&) .) . flip (map . flip id))
 
applyNTimes f n x = iterate f x !! n

main = liveMain

liveMain = do
       hSetBuffering stdin  NoBuffering
      -- hSetBuffering stdout NoBuffering
       let subBreak = putStrLn $ replicate 5 '^'
       let thingy state = putStr $ applyAll (makeTextCanvas 30) (map (\[x,y] -> replaceAtText '#' (x,y)) state)
       let t state = filter (\x -> (((==3).(countNeighbors state)) x) || ((((flip elem [3,4]).(countNeighbors state)) x) && (elem x state))  ) $ nub $ concatMap neighboringAreas state
       forM (iterate t initialState) $ \x -> do
         system "clear" 
         thingy x
         getChar
         subBreak
