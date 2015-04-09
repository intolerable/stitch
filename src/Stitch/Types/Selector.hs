module Stitch.Types.Selector where

import Data.Text (Text)
import Data.Monoid
import Data.String
import qualified Data.Text as Text

-- | Represents a CSS selector. Can be combined with other 'Selector's using its 'Monoid' instance.
newtype Selector = Selector Text
  deriving (Show, Read, Eq, Ord)

instance IsString Selector where
  fromString = Selector . fromString

instance Monoid Selector where
  mempty = ""
  Selector "" `mappend` Selector y = Selector y
  Selector x `mappend` Selector "" = Selector x
  Selector x `mappend` Selector y =
    if "&" `Text.isInfixOf` y
      then
        Selector (Text.replace "&" x y)
      else
        Selector (x <> " " <> y)
