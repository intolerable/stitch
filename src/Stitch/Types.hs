-- | Defines all the types needed for "Stitch"'s internal CSS representation. You shouldn't need to import this module unless you're messing around with the 'Block' representation before outputting CSS.
module Stitch.Types
  ( Selector(..)
  , Children(..)
  , Property(..)
  , Import(..)
  , Block(..)
  , InnerBlock(..) ) where

import Stitch.Types.Selector

import Data.Map (Map)
import Data.Text (Text)
import Data.Semigroup (Semigroup(..))
import qualified Data.Map as Map

-- | Children is a simple specialized wrapper around 'Map' with a custom 'Monoid' instance. Instead of simply 'Data.Map.union'ing the two 'Map's, the 'Children' instance 'mappend's the two values together in case of a key clash.
newtype Children = Children (Map Selector InnerBlock)
  deriving (Show, Read, Eq)

instance Semigroup Children where
  Children x <> Children y =
    Children (Map.unionWith mappend x y)

instance Monoid Children where
  mempty = Children mempty
  mappend = (<>)


-- | Type for a CSS property or comment. The two are combined because we want to keep the ordering of comments and properties in the output CSS.
data Property = Property Text Text
              | Comment Text
  deriving (Show, Read, Eq)

-- | Basic newtype for handling css @@import@ statements.
newtype Import = Import Text
  deriving (Show, Read, Eq)

-- | Representation of a CSS inner block. Similar to a top-level 'Block', but doesn't allow 'Import's.
data InnerBlock = InnerBlock [Property] Children
  deriving (Show, Read, Eq)

instance Semigroup InnerBlock where
  InnerBlock p c <> InnerBlock q d =
    InnerBlock (p <> q) (c <> d)

instance Monoid InnerBlock where
  mempty = InnerBlock [] mempty
  mappend = (<>)

-- | Top-level representation of a CSS document.
data Block = Block [Import] [Property] Children
  deriving (Show, Read, Eq)

instance Semigroup Block where
  Block i p c <> Block j q d =
    Block (i <> j) (p <> q) (c <> d)

instance Monoid Block where
  mempty = Block mempty mempty mempty
  mappend = (<>)
