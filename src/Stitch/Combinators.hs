-- | This module defines most of the functions that are core to the DSL. Most of them are re-exported by "Stitch", so you usually shouldn't need to import this module directly (unless you want to use CSS imports).
module Stitch.Combinators
  ( (.=)
  , (?)
  , (-:)
  , comment
  , cssImport ) where

import Control.Monad.Trans.Stitch
import Stitch.Types

import Control.Monad.Trans.Writer.Strict
import Data.Monoid (Monoid(..))
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Add a key-value property pair. For example, @"color" .= "red"@ will add @color: red@ to the CSS output.
(.=) :: Monad m => Text -> Text -> StitchT m ()
k .= v = StitchT $ tell $ Block [] [Property k v] mempty
infix 8 .=

-- | Nest a selector under the current selector. For example, this:
--
-- > "h1" ? do
-- >   "color" .= "red"
-- >   "a" ?
-- >     "text-decoration" .= "underline"
--
-- | results in the following being added to the CSS output:
--
-- > h1 {
-- >   color: red
-- > }
-- > h1 a {
-- >   text-decoration: underline
-- > }
(?) :: Monad m => Selector -> StitchT m a -> StitchT m a
sel ? (StitchT x) = StitchT $ censor (\(Block is ps cs) -> Block is [] (Children $ Map.singleton sel (InnerBlock ps cs))) x
infixr 6 ?

-- | @"pref" -: assignments@ prefixes all the keys of @assignments@ with @pref-@. This can be useful for putting a bunch of grouped "font" or "border" properties together – for example, the following two actions function the same:
--
-- > "font" -: do
-- >   "size" .= "1.5rem"
-- >   "family" .= "Helvetica"
-- >   "weight" .= "bold"
--
-- > "font-size" .= "1.5rem"
-- > "font-family" .= "Helvetica"
-- > "font-weight" .= "bold"
(-:) :: Monad m => Text -> StitchT m a -> StitchT m a
prefix -: (StitchT x) =
  StitchT $ censor (\(Block _ ps cs) -> Block [] (map (prefixProperty prefix) ps) (prefixChildren prefix cs)) x
infixr 6 -:

prefixProperty :: Text -> Property -> Property
prefixProperty pref (Property k v) = Property (if Text.null k then pref else pref <> "-" <> k) v
prefixProperty _ x = x

prefixChildren :: Text -> Children -> Children
prefixChildren pref (Children x) = Children (fmap (prefixInnerBlock pref) x)

prefixInnerBlock :: Text -> InnerBlock -> InnerBlock
prefixInnerBlock pref (InnerBlock ps cs) =
  InnerBlock (fmap (prefixProperty pref) ps)
             (prefixChildren pref cs)

-- | Add a comment to the CSS output. The 'Stitch.Render.compressed' printer won't add comments to the final CSS output.
comment :: Monad m => Text -> StitchT m ()
comment c = StitchT $ tell $ Block [] [Comment c] mempty

-- | Add an @\@import@ statement to the top-level of the CSS output.
cssImport :: Monad m => Text -> StitchT m ()
cssImport u = StitchT $ tell $ Block [Import u] [] mempty
