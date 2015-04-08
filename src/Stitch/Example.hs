-- | An example of how to use "Stitch".
module Stitch.Example where

import Stitch
import Stitch.Combinators

import Data.Text (Text)

-- | An example of a CSS document defined programmatically using "Stitch". To convert this to an real CSS document, use 'renderCSS'.
exampleCSS :: CSS
exampleCSS = do
  cssImport "url(http://fonts.googleapis.com/css?family=Lato)"
  "body" ? do
    "background-color" .= "#dddddd"
    "color" .= "#000000"
    "h1" ? do
      "color" .= "green"
  "h1" ?
    "font-weight" .= "bold"

-- | An example of "Stitch"'s output. Uses the 'Stitch.Renderer.basic' printer.
exampleOutput :: Text
exampleOutput = renderCSS exampleCSS
