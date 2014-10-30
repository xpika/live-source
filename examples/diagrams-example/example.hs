import LiveSource

main = do
       repeatOnModification "source.hs"
       putStrLn "end"
