module StitchSpec where

import Stitch
import Stitch.Combinators

import Test.Hspec
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  match "empty.css" $ return ()
  match "basic_import.css" $
    cssImport "empty.css"
  match "basic_props.css" $
    "body" ?
      "color" .= "red"
  match "multiple_props.css" $
    "body" ? do
      "color" .= "red"
      "background-color" .= "blue"
  match "basic_nested_props.css" $
    "body" ? do
      "color" .= "red"
      "h1" ?
        "font-size" .= "200%"

match :: FilePath -> CSS -> SpecWith ()
match fn css = describe fn $
  it "matches the rendered css" $ do
    file <- Text.readFile ("test" </> "css" </> fn)
    renderCSS css `shouldBe` Text.stripEnd file

(</>) :: FilePath -> FilePath -> FilePath
x </> y = x ++ "/" ++ y
