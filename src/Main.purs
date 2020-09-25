module Main where

import Prelude

import App (mkApp)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.Hooks (element)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
    go =<< getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  where go :: Maybe _ -> Effect Unit
        go Nothing = throw "Container element not found"
        go (Just c) = do
          app <- mkApp
          render (element app {}) c
