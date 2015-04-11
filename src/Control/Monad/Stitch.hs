-- | This module exports the 'Stitch' type, which is simply 'StitchT' parameterized over 'Identity', for computations which don't require any other monadic capabilities.
module Control.Monad.Stitch
  ( Stitch
  , CSS
  , runStitch ) where

import Control.Monad.Trans.Stitch
import Stitch.Types

import Data.Functor.Identity

-- | The 'StitchT' monad transformer specialized to 'Identity'. This will typically be the Stitch variant used since it doesn't do anything special.
type Stitch = StitchT Identity

-- | Abstract representation of a CSS document. This can be transformed to an actual CSS document with 'Stitch.Render.renderCSS'.
type CSS = Stitch ()

-- | Evaluate a computation in the 'Stitch' monad, returning computation's value and a concrete representation of the CSS document.
runStitch :: Stitch a -> (a, Block)
runStitch = runIdentity . runStitchT
