module Control.Monad.Trans.Stitch
  ( StitchT(..)
  , runStitchT ) where

import Stitch.Types

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Data.Monoid

newtype StitchT m a = StitchT (WriterT Block m a)
  deriving (Functor, Applicative, Monad, Alternative, MonadIO, MonadTrans)

instance (Applicative m, Monoid a) => Monoid (StitchT m a) where
  mempty = pure mempty
  a `mappend` b = mappend <$> a <*> b

runStitchT :: Monad m => StitchT m a -> m (a, Block)
runStitchT (StitchT x) = runWriterT x
