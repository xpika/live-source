import System.Cmd


main = do
       x <- system "ghc -package ghc live-io-example.hs -e \":main \""
       return ()