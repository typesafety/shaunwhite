-- | Provides a custom Prelude for the project. This module allows us to tweak
-- the contents of Relude by hiding or adding content.

module CustomPrelude (
    module CustomPrelude,
    module ReludeLess,
    ) where

import Relude as ReludeLess hiding (
    -- Hide RWS stuff that is replaced by Polysemy equivalents
    State, runState, evalState, execState, get, put, state,
    Reader, runReader, ask, asks, local, reader, withReader,
    )


{-# WARNING todo "Unhandled TODO placeholder expression." #-}
todo :: a
todo = error "ERROR: TODO IN CODE"

{- | Append two type-level lists. Taken from the source code of:
https://hackage.haskell.org/package/type-combinators-0.2.4.3
-}
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
    '[]       ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)
infixr 5 ++
