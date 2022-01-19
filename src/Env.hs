{-# LANGUAGE TemplateHaskell #-}

module Env (
    -- * The program state while running
    Env (..),
    envRequestableRoles,

    envFromCfg,
    ) where

import CustomPrelude

import Control.Lens

import Config (Cfg, cfgAvailRoles)


-- | Contains necessary state for the program while running.
data Env = Env
    { _envRequestableRoles :: Set Text
    -- ^ Names of roles that can be requested.
    -- TODO: possibly use some other means of identification of roles, such
    --       as the role ID (:: Snowflake Role).
    } deriving (Show)
$(makeLenses ''Env)

envFromCfg :: Cfg -> Env
envFromCfg cfg = Env
    { _envRequestableRoles = cfg ^. cfgAvailRoles
    }
