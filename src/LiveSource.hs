module LiveSource where

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

newMain = do
       args <- getArgs
       case args of
            [filePath] ->  repeatOnMofication filePath
            _ -> print "usage: filename_to_repeat_loading"

repeatOnMofication filePath = repeatOnMofication' Nothing filePath

	   
repeatOnMofication' lastedModified filePath = do
       lastModified' <- getModificationTime filePath
       case lastedModified of 
        (Just a) -> case a < lastModified' of 
            True -> loadAndRunFilePrintingErrorMessage filePath
            _ -> return Nothing
        _ -> loadAndRunFilePrintingErrorMessage filePath
       threadDelay 10000
       repeatOnMofication' (Just lastModified') filePath

ntry :: IO a -> IO (Either SomeException a)	   
ntry = try

loadAndRunFilePrintingErrorMessage filePath = do
       res <- ntry (loadAndRunFile filePath)
       case  res  of
         Left msg -> print msg >> return Nothing
         Right v -> return (Just v)

loadAndRunFile filePath = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    oldcwd <- getCurrentDirectory
    res <- runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget filePath Nothing]
        load LoadAllTargets
        setContext [IIModule $ mkModuleName "Main"]
        act <- unsafeCoerce <$> compileExpr "liveMain"
        g <- liftIO act
        return g
    setCurrentDirectory oldcwd
    return res
	