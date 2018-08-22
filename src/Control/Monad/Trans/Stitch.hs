module Control.Monad.Trans.Stitch
  ( StitchT(..)
  , runStitchT ) where

import Stitch.Types

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))

newtype StitchT m a = StitchT (WriterT Block m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadIO, MonadTrans)

instance (Applicative m, Semigroup a) => Semigroup (StitchT m a) where
  a <> b = (<>) <$> a <*> b

instance (Applicative m, Monoid a, Semigroup a) => Monoid (StitchT m a) where
  mempty = pure mempty
  mappend = (<>)

runStitchT :: Monad m => StitchT m a -> m (a, Block)
runStitchT (StitchT x) = runWriterT x
