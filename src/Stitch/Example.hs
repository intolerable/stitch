-- | An example of how to use "Stitch".
module Stitch.Example where

import Stitch
import Stitch.Combinators

import Data.Text (Text)

-- | An example of a CSS document defined programmatically using "Stitch". To convert this to an real CSS document, use 'renderCSS'.
exampleCSS :: CSS
exampleCSS = do
  cssImport "url(http://fonts.googleapis.com/css?family=Lato)"
  comment "This is an example comment. The \"compressed\" printer automatically strips comments to save space"
  "body" ? do
    "background-color" .= "#dddddd"
    "color" .= "#000000"
    "h1" ? do
      "color" .= "green"
    "h2" ? do
      "a:hover" ? do
        "color" .= "blue"
  "h1" ?
    "font-weight" .= "bold"

-- | An example of "Stitch"'s output. Uses the 'Stitch.Renderer.basic' printer.
exampleOutput :: Text
exampleOutput = renderCSS exampleCSS
