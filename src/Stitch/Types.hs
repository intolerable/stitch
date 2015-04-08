module Stitch.Types
  ( module Stitch.Types.Selector
  , Children(..)
  , Property(..)
  , Block(..) ) where

import Stitch.Types.Selector

import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Map as Map

newtype Children = Children (Map Selector Block)
  deriving (Show, Read, Eq)

instance Monoid Children where
  mempty = Children mempty
  Children x `mappend` Children y =
    Children (Map.unionWith mappend x y)

data Property = Property Text Text
              | Comment Text
              | Import Text
  deriving (Show, Read, Eq)

data Block = Block [Property] Children
  deriving (Show, Read, Eq)

instance Monoid Block where
  mempty = Block mempty mempty
  Block p c `mappend` Block q d =
    Block (p <> q) (c <> d)
