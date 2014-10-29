live-source
===========

basic use: 

1. create client haskell script without a module name
2. give the script a function named liveMain

eg :
```haskell
 -- file test.hs
 liveMain = print 2

```


3. open ghci

```haskell
Prelude> import LiveSource
Prelude LiveSource> repeatOnModification "test.hs"
2
```

4. edit file test.hs and save it

eg :
```haskell
 -- file test.hs
 
liveMain = print (take 10 [1..])
```

Ghc should now run the IO.

```haskell
Prelude> import LiveSource
Prelude LiveSource> repeatOnModification "test.hs"
2
[1,2,3,4,5,6,7,8,9,10]

```

For more advanced usage see examples
