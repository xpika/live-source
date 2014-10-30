{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String

qqq =  renderHtml  (numbers) ::  String

numbers =  H.docTypeHtml $ do
    H.head $ do
         H.title "Natural numbers"
    body $ do
         p "A list of natural numbers:"
         ul $ forM_ ([1 .. 9]::[Int]) (li . toHtml)
         H.span $ toHtml ("foo" :: String)

liveMain = do
    putStr ""
    let value = qqq
    return value