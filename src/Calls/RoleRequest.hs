{- |
Request to be added to a role, add or delete requestable roles
-}
module Calls.RoleRequest
       ( cmdRoleRequestAdd
       , cmdRoleRequestDel
    --    , cmdRoleRequestReq
       ) where

import Discord
-- import Discord (DiscordHandler, RestCallErrorCode, restCall)
import Discord.Types
import Discord.Requests

import Env (CmdEnv (..))

import qualified Data.Set as S

import qualified Auth
import qualified Env

-- * Commands

cmdRoleRequestAdd :: [Text] -> Env.Shaun DiscordHandler (Either RestCallErrorCode ())
cmdRoleRequestAdd = addDel addRoles
  where
    addRoles :: Set Text -> Env.Shaun DiscordHandler ()
    addRoles roles = Env.modEnv $ \ cmdEnv ->
        cmdEnv { cmdEnvRequestableRoles = S.union roles $ cmdEnvRequestableRoles cmdEnv }

cmdRoleRequestDel :: [Text] -> Env.Shaun DiscordHandler (Either RestCallErrorCode ())
cmdRoleRequestDel = addDel delRoles
  where
    delRoles :: Set Text -> Env.Shaun DiscordHandler ()
    delRoles roles = Env.modEnv $ \ cmdEnv ->
        cmdEnv { cmdEnvRequestableRoles = cmdEnvRequestableRoles cmdEnv S.\\ roles }

-- * Helper functions for generalization of add/del operations.

addDel
    :: (Set Text -> Env.Shaun DiscordHandler ())
               -- ^ Action to perform (add/delete), given the role names.
    -> [Text]  -- ^ Names of roles to add/delete.
    -> Env.Shaun DiscordHandler (Either RestCallErrorCode ())
addDel action roles = do
    Just msg <- Env.getMsg
    Auth.authAndRunWithMsg msg () $ do
        Right gRoles <- guildRolesFromMsg msg

        let validRoleNames :: Set Text
            validRoleNames =
                S.intersection (S.fromList roles) (S.fromList $ map roleName gRoles)

        action validRoleNames

        pure . Right $ ()

guildRolesFromMsg
    :: Message
    -> Env.Shaun DiscordHandler (Either RestCallErrorCode [Role])
guildRolesFromMsg msg = do
    Just gId <- pure $ messageGuild msg
    lift $ restCall (GetGuildRoles gId)
