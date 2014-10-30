{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.Trans
import Data.Text.Lazy
import Language.Haskell.Interpreter hiding (get)
import LiveSource

import Data.Monoid (mconcat)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    maybeTextoutput <- liftIO $ loadAndRunFilePrintingErrorMessage (unpack beam)
    let textoutput = case maybeTextoutput of 
                        (Just to) -> to
                        _ ->  ""
    html $ mconcat [pack textoutput]

    
