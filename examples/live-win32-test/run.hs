import System.Cmd


main = do
       x <- system "ghci -package ghc Win32Example.hs"
       return ()