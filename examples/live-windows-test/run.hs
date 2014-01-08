import System.Cmd


main = do
       x <- system "ghci -package ghc WindowsExample.lhs"
       return ()