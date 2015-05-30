-- | Importing this module should bring enough functions into scope to use the Stitch DSL for most purposes.
module Stitch
  (
  -- * Core
    CSS
  , renderCSS
  , printCSS
  , Selector(..)
  -- * Combinators
  , (?)
  , (.=)
  , comment ) where

import Control.Monad.Stitch
import Stitch.Render
import Stitch.Types
import Stitch.Combinators
