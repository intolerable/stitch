module Stitch.Types
  ( Selector(..)
  , Children(..)
  , Property(..)
  , Import(..)
  , Block(..)
  , InnerBlock(..) ) where

import Stitch.Types.Selector

import Data.Map (Map)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Map as Map

newtype Children = Children (Map Selector InnerBlock)
  deriving (Show, Read, Eq)

instance Monoid Children where
  mempty = Children mempty
  Children x `mappend` Children y =
    Children (Map.unionWith mappend x y)

data Property = Property Text Text
              | Comment Text
  deriving (Show, Read, Eq)

newtype Import = Import Text
  deriving (Show, Read, Eq)

data InnerBlock = InnerBlock [Property] Children
  deriving (Show, Read, Eq)

instance Monoid InnerBlock where
  mempty = InnerBlock [] mempty
  InnerBlock p c `mappend` InnerBlock q d =
    InnerBlock (p <> q) (c <> d)

data Block = Block [Import] [Property] Children
  deriving (Show, Read, Eq)

instance Monoid Block where
  mempty = Block mempty mempty mempty
  Block i p c `mappend` Block j q d =
    Block (i <> j) (p <> q) (c <> d)
