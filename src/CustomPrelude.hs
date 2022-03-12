-- | Provides a custom Prelude for the project. This module allows us to tweak
-- the contents of Relude by hiding or adding content.

-- TODO: Add commonly used useful stuff:
--       Polysemy types and effects, common functions from Lens etc.
-- TODO: Use Lazy text by default (maybe?) for better compatibility with Calamity.

module CustomPrelude (
    module CustomPrelude,
    module ReludeLess,

    -- Prettyprinter for debugging
    module Debug.Pretty.Simple
    ) where

import Data.Text.IO qualified as T
import Debug.Pretty.Simple
import Relude as ReludeLess hiding (
    -- Hide RWS stuff that is replaced by Polysemy equivalents
    State, runState, evalState, execState, get, put, modify, modify', state,
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

-- | Like "putTextLn", but outputs to stderr.
errTextLn :: MonadIO m => Text -> m ()
errTextLn = liftIO . T.hPutStrLn stderr
