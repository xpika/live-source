module Main where 
import Control.Applicative
import DynFlags
import GHC
import GHC.Paths
import GhcMonad (liftIO)
import Unsafe.Coerce
import Control.Exception

import Control.Concurrent
import System.Directory
import System.Time
import System.Environment

main = do
       args <- getArgs
       case args of
            [filePath] ->  repeatOnMofication Nothing filePath
            _ -> print "usage: filename_to_repeat_loading"

repeatOnMofication lastedModified filePath = do
       lastModified' <- getModificationTime filePath
       case lastedModified of 
        (Just a) -> case a < lastModified' of 
            True -> exceptionToEither filePath
            _ -> return ()
        _ -> exceptionToEither filePath
       threadDelay 10000
       repeatOnMofication (Just lastModified') filePath
        
exceptionToEither filePath = do
       res <- try (loadAndRunFile filePath)
       case  res ::  Either SomeException () of
         Left msg -> print msg
         Right _ -> return ()

loadAndRunFile filePath = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget filePath Nothing]
        load LoadAllTargets
        setContext [IIModule $ mkModuleName "Test"]
        act <- unsafeCoerce <$> compileExpr "main"
        g <- liftIO act
        liftIO $ print (g :: ())