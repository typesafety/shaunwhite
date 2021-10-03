module Rolerequest (
    makeRequestable,
    revokeRequestable,
    rolerequest,
    ) where

import CustomPrelude

import Calamity (BotC, Guild, Role)
import Calamity qualified as C
import Calamity.Commands.Context (FullContext)
import Control.Lens
import Data.Set qualified as Set
import DiPolysemy (info, warning)
import Polysemy (Member, Members, Sem)
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import Polysemy.State qualified as S

import Env (Env, envRequestableRoles)


--
-- * Bot commands
--

{- | Give the user issuing the bot command the role with the corresponding name.
The request is only granted if the role is in the explicit list of requestable
roles.
-}
rolerequest :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
rolerequest ctxt roleName = do
    Just guild <- pure . view #guild $ ctxt
    Just role <- lookupRole guild roleName

    available <- S.gets (view envRequestableRoles)
    when (view #name role `Set.member` available)
        $ void . C.invoke $ C.AddGuildMemberRole guild (ctxt ^. #user) role

-- | Make the given role requestable with 'rolerequest'.
makeRequestable :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
makeRequestable ctxt roleName = do
    Just guild <- pure . view #guild $ ctxt
    lookupRole guild roleName >>= \case
        Nothing -> warning @Text $
            "Tried making non-existing role requestable: " <> roleName
        Just _role -> do
            S.modify' $ over envRequestableRoles (Set.insert $ toLazy roleName)
            available <- S.gets (view envRequestableRoles)
            info @Text $ "Made role `" <> roleName <> "` requestable"
            info @Text $ "Requestable roles are now: " <> show available

-- | Revoke requestable status of the given role.
revokeRequestable :: forall r . (BotC r, Members '[Fail, State Env] r)
    => Text -> Sem r ()
revokeRequestable roleName = do
    available <- S.gets (view envRequestableRoles)
    if toLazy roleName `Set.member` available
        then do
            S.modify' $ over envRequestableRoles (Set.delete $ toLazy roleName)
            availableAfter <- S.gets (view envRequestableRoles)
            info @Text $ "Removed `" <> roleName <> "` from requestable roles"
            info @Text $ "Requestable roles are now: " <> show availableAfter
        else warning @Text $ "Tried to revoke already non-requestable role: " <> roleName

--
-- * Helper functions
--

-- | Return the role of the given name if it exists in the guild, or Nothing otherwise.
lookupRole :: (BotC r, Member Fail r) => Guild -> Text -> Sem r (Maybe Role)
lookupRole guild roleName = do
    Right roles <- C.invoke (C.GetGuildRoles guild)
    pure $ roleFromName roleName roles
  where
    roleFromName :: Text -> [Role] -> Maybe Role
    roleFromName name = find ((== name) . toStrict . view #name)
