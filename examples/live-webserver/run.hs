import System.Cmd


main = do
       x <- system "ghc -package ghc live-webserver-example.hs -e \":main \""
       return ()