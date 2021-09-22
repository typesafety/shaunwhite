-- | Provides a custom Prelude for the project. This module allows us to tweak
-- the contents of Relude by hiding or adding content.

module CustomPrelude (
    module CustomPrelude,
    module ReludeLess,
    ) where

import Relude as ReludeLess hiding (
    -- Hide MTL stuff that is replaced by Polysemy equivalents
    State,
    runState
    )


{-# WARNING todo "Unhandled TODO placeholder expression." #-}
todo :: a
todo = error "ERROR: TODO IN CODE"
