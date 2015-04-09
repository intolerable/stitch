module Stitch.Combinators
  ( (.=)
  , (?)
  , (-:)
  , comment
  , cssImport ) where

import Control.Monad.Trans.Stitch
import Stitch.Types

import Control.Monad.Trans.Writer.Strict
import Data.Monoid
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as Text

-- | Add a key-value property pair.
(.=) :: Monad m => Text -> Text -> StitchT m ()
k .= v = StitchT $ tell $ Block [] [Property k v] mempty
infix 7 .=

-- | Nest a selector under the current selector.
(?) :: Monad m => Selector -> StitchT m a -> StitchT m a
sel ? (StitchT x) = StitchT $ censor (\(Block is ps cs) -> Block is [] (Children $ Map.singleton sel (InnerBlock ps cs))) x
infixr 6 ?

(-:) :: Monad m => Text -> StitchT m a -> StitchT m a
prefix -: (StitchT x) =
  StitchT $ censor (\(Block _ ps _) -> Block [] (map (prefixProperty prefix) ps) mempty) x

prefixProperty :: Text -> Property -> Property
prefixProperty pref (Property k v) = Property (if Text.null k then pref else pref <> "-" <> k) v
prefixProperty _ x = x

-- | Add a comment to the CSS output.
comment :: Monad m => Text -> StitchT m ()
comment c = StitchT $ tell $ Block [] [Comment c] mempty

cssImport :: Monad m => Text -> StitchT m ()
cssImport u = StitchT $ tell $ Block [Import u] [] mempty
