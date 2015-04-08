module Stitch.Render
  ( BlockPrinter
  , renderCSS
  , renderCSSWith
  , renderStitchTWith
  , basic
  , compressed ) where

import Control.Monad.Stitch
import Control.Monad.Trans.Stitch
import Stitch.Types

import Data.Monoid
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text

type BlockPrinter = ([Selector] -> Block -> Text)

-- | Convert an abstract 'CSS' document to a real CSS document.
renderCSS :: CSS -> Text
renderCSS = renderCSSWith basic

-- | Convert an abstract 'CSS' document to a real CSS document using a specific printer. A simple printer called 'basic' is included, as well as a compressing printer called 'compressed'.
renderCSSWith :: BlockPrinter -> CSS -> Text
renderCSSWith f c = f [] $ snd $ runStitch c

renderStitchTWith :: Monad m => BlockPrinter -> StitchT m () -> m Text
renderStitchTWith f s = do
  (_, block) <- runStitchT s
  return $ f [] block

-- | Outputs a basic human-readable version of the CSS document. Line breaks are added between blocks, and properties are spaced nicely.
basic :: BlockPrinter
basic [] (Block _ cs) =
  Text.intercalate "\n" $ collectChildren basic [] cs
basic selectors (Block ps cs) =
  case map basicProp ps of
    [] -> Text.intercalate " " $ collectChildren basic selectors cs
    props -> Text.intercalate "\n" $ Text.intercalate " "
      [ unSelector $ mconcat $ reverse selectors
      , "{"
      , Text.intercalate "; " props
      , "}" ] : collectChildren basic selectors cs
  where

collectChildren :: BlockPrinter -> [Selector] -> Children -> [Text]
collectChildren bp selectors (Children cs) =
  map (\(k, v) -> bp (k:selectors) v) $ Map.toList cs

basicProp :: Property -> Text
basicProp (Comment t) = mconcat ["/* ", t, " */"]
basicProp (Property k v) = mconcat [k, ": ", v]
basicProp (Import u) = mconcat ["@import ", u]

-- | A minimal printer that aims for tiny output CSS. All spaces are removed.
compressed :: BlockPrinter
compressed [] (Block ps cs) =
  mconcat $ map ((<> ";") . compressedProp) ps <> collectChildren compressed [] cs
compressed selectors (Block ps cs) =
  case map compressedProp ps of
    [] -> mconcat $ collectChildren compressed selectors cs
    props -> Text.intercalate "" $
      [ Text.intercalate " " $ reverse $ map unSelector selectors
      , "{"
      , Text.intercalate ";" props
      , "}" ] <> collectChildren compressed selectors cs

compressedProp :: Property -> Text
compressedProp (Comment _) = mempty
compressedProp (Property k v) = mconcat [k, ":", v]
compressedProp (Import u) = mconcat ["@import ", u]

unSelector :: Selector -> Text
unSelector (Selector t) = t
