import System.Cmd


main = do
       x <- system "ghc -package ghc loader.hs -e \":main test.hs\""
       return ()