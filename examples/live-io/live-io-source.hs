module Test where

import System.Cmd

liveMain = do
		    -- system "cls"
		   print $ map (*1000) $  take 7 [1..]

