-- | This module contains all the functions needed to convert a CSS document from its internal representation ('Block') to a concrete text-format CSS output.
module Stitch.Render
  ( renderCSS
  , printCSS
  , BlockPrinter
  , renderCSSWith
  , renderStitchTWith
  , basic
  , compressed ) where

import Control.Monad.Stitch
import Control.Monad.Trans.Stitch
import Stitch.Types

import Data.Monoid (Monoid(..))
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- | Type of the CSS printers – a function from the internal 'Block' representation of the CSS to a concrete 'Text' output.
type BlockPrinter = (Block -> Text)
type InnerBlockPrinter = (Selector -> InnerBlock -> Text)

-- | Convert an abstract 'CSS' document to a real CSS document.
renderCSS :: CSS -> Text
renderCSS = renderCSSWith basic

printCSS :: CSS -> IO ()
printCSS = Text.putStrLn . renderCSS

-- | Convert an abstract 'CSS' document to a real CSS document using a specific printer. A simple printer called 'basic' is included, as well as a compressing printer called 'compressed'.
renderCSSWith :: BlockPrinter -> CSS -> Text
renderCSSWith f c = f $ snd $ runStitch c

-- | Convert an abstract 'CSS' document built with the 'StitchT' transformer into its monad-wrapped 'Text' output. Unless you're explicitly using the transformer, this function is probably not useful.
renderStitchTWith :: Monad m => BlockPrinter -> StitchT m () -> m Text
renderStitchTWith f s = do
  (_, block) <- runStitchT s
  return $ f block

-- | Outputs a basic human-readable version of the CSS document. Line breaks are added between blocks, and properties are spaced nicely.
basic :: BlockPrinter
basic (Block is ps cs) =
  Text.intercalate "\n" $ map basicPropTL ps <> map ((<> ";") . basicImport) is <> collectChildren basicInner mempty cs

basicInner :: InnerBlockPrinter
basicInner selectors (InnerBlock [] cs) =
  Text.intercalate " " $ collectChildren basicInner selectors cs
basicInner selectors (InnerBlock ps cs) =
  if all (Text.isInfixOf "&") $ unSelector selectors
    then
      Text.intercalate "\n" $ collectChildren basicInner selectors cs
    else
      Text.intercalate "\n" $ mconcat
        [ Text.intercalate ", " $ filter (not . Text.isInfixOf "&") $ unSelector selectors
        , " {\n  "
        , Text.intercalate ";\n  " $ map basicProp ps
        , "\n}" ] : collectChildren basicInner selectors cs

basicImport :: Import -> Text
basicImport (Import i) = mconcat ["@import ", i]

basicProp :: Property -> Text
basicProp (Comment t) = mconcat ["/* ", Text.replace "\n" "\n  " t, " */"]
basicProp (Property k v) = mconcat [k, ": ", v]

basicPropTL :: Property -> Text
basicPropTL (Comment t) = mconcat ["/* ", t, " */"]
basicPropTL _ = mempty

collectChildren :: InnerBlockPrinter -> Selector -> Children -> [Text]
collectChildren ibp selectors (Children cs) =
  map (\(k, v) -> ibp (selectors <> k) v) $ Map.toList cs

-- | A minimal printer that aims for tiny output CSS. All spaces are removed.
compressed :: BlockPrinter
compressed (Block is _ cs) =
  mconcat $ map ((<> ";") . compressedImport) is <> collectChildren compressedInner mempty cs

compressedInner :: InnerBlockPrinter
compressedInner selectors (InnerBlock [] cs) =
  mconcat $ collectChildren compressedInner selectors cs
compressedInner selectors (InnerBlock ps cs) =
  if all (Text.isInfixOf "&") $ unSelector selectors
    then
      mconcat $collectChildren compressedInner selectors cs
    else
      mconcat $ mconcat
        [ Text.intercalate "," $ filter (not . Text.isInfixOf "&") $ unSelector selectors
        , "{"
        , Text.intercalate ";" $ map compressedProp ps
        , "}" ] : collectChildren compressedInner selectors cs

compressedProp :: Property -> Text
compressedProp (Comment _) = mempty
compressedProp (Property k v) = mconcat [k, ":", v]

compressedImport :: Import -> Text
compressedImport = basicImport
