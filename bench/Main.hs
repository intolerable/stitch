module Main where

import Stitch
import Stitch.Render

import Control.Monad
import Criterion.Main
import Data.Monoid
import Prelude

main :: IO ()
main = defaultMain
  [ bgroup "css"
    [ bench "basic" $ nf renderCSS example
    , bench "compressed" $ nf (renderCSSWith compressed) example ] ]

example :: CSS
example =
  forM_ [1..1000] $ \n -> do
    mconcat (replicate n "x") ?
      "color" .= "green"
