{-# LANGUAGE TemplateHaskell #-}

module Env (
    -- * The program state while running
    Env (..),
    envAvailRoles,
    ) where

import CustomPrelude

import Control.Lens.TH (makeLenses)
import Data.Text.Lazy qualified as L


-- | Contains necessary state for the program while running.
data Env = Env
    { _envAvailRoles :: [L.Text]
    -- ^ Names of roles that can be requested.
    -- TODO: possibly use some other means of identification of roles, such
    --       as the role ID (:: Snowflake Role).
    } deriving (Show)
$(makeLenses ''Env)
