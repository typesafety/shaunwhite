{-# LANGUAGE TemplateHaskell #-}

module Env (
    -- * The program state while running
    Env (..),
    envAvailRoles,

    envFromCfg,
    ) where

import CustomPrelude

import Control.Lens
import Data.Text.Lazy qualified as L

import Config (Cfg, cfgAvailRoles)


-- | Contains necessary state for the program while running.
data Env = Env
    { _envAvailRoles :: [L.Text]
    -- ^ Names of roles that can be requested.
    -- TODO: possibly use some other means of identification of roles, such
    --       as the role ID (:: Snowflake Role).
    } deriving (Show)
$(makeLenses ''Env)

envFromCfg :: Cfg -> Env
envFromCfg cfg = Env
    { _envAvailRoles = cfg ^. cfgAvailRoles
    }
