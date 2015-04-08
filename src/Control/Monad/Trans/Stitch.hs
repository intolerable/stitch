module Control.Monad.Trans.Stitch
  ( StitchT(..)
  , runStitchT ) where

import Stitch.Types

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.Monoid
import Control.Monad

newtype StitchT m a = StitchT (WriterT Block m a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, Monoid a) => Monoid (StitchT m a) where
  mempty = return mempty
  a `mappend` b = liftM2 mappend a b

runStitchT :: Monad m => StitchT m a -> m (a, Block)
runStitchT (StitchT x) = runWriterT x
