{-# LANGUAGE QuasiQuotes #-}

module Rolerequest (
    registerRolesCommands,
    rolerequest,
    ) where

import CustomPrelude

import Calamity (BotC, Guild, Role)
import Calamity qualified as C
import Calamity.Commands qualified as C
import Calamity.Commands.Context (FullContext)
import Control.Lens
import Data.Set qualified as Set
import Data.Text.Lazy qualified as L
import Data.Vector.Unboxing qualified as Vec
import DiPolysemy (debug, info, warning)
import Polysemy (Member, Members, Sem)
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import Polysemy.State qualified as S
import PyF (fmt)

import Auth (registerAdminCmd)
import Env (Env, envRequestableRoles)


--
-- * Bot commands
--

{- | Register commands under the group "roles". Registers commands such as
"roles request" and "roles list".
-}
registerRolesCommands :: forall r . (BotC r, Members '[State Env] r)
    => Sem (C.DSLState FullContext r) ()
registerRolesCommands = C.help (const rolesHelp) $ C.group' "roles" $ do
    -- Request to be assigned a role from the list of requestable roles.
    void $ C.help (const rolerequestHelp) $ C.command @'[Text] "request" rolerequest

    void $ C.help (const leaveRoleHelp) $ C.command @'[Text] "leave" leaveRole

    -- Show the list of requestable roles.
    void
        $ C.help (const listRequestableHelp)
        $ C.command @'[] "list-requestable" listRequestable

    -- ADMIN: Add a role as requestable.
    registerAdminCmd . C.hide $ C.command @'[Text] "make-requestable" makeRequestable

    -- ADMIN: Revoke a role as requestable.
    registerAdminCmd . C.hide $ C.command @'[Text] "revoke-requestable" revokeRequestable

    -- ADMIN: Give a role to everyone.
    registerAdminCmd . C.hide $ C.command @'[Text] "give-all" roleToAll


roleToAll :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
roleToAll ctxt roleName = case view #guild ctxt of
    Nothing -> warning @Text $ "Command must be run in a guild."
    Just g -> do
        Just role <- lookupRole g roleName
        Right members <-
            C.invoke $ C.ListGuildMembers g (C.ListMembersOptions (Just 999) Nothing)

        void . C.tell ctxt $ "Giving EVERYONE role: " <> roleName
        mapM_ (\m -> C.invoke $ C.AddGuildMemberRole g m role) members

-- TODO: Add command for removing role from everyone

{- | Give the user issuing the bot command the role with the corresponding name.
The request is only granted if the role is in the explicit list of requestable
roles.
-}
rolerequest :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
rolerequest ctxt roleName = case view #member ctxt of
    Nothing -> warning @Text
        $ "Could not find command invoker to be a guild member."
        <> " Check that the bot has privileged intents enabled."
    Just member -> do
        Just guild <- pure . view #guild $ ctxt
        Just role <- lookupRole guild roleName
        available <- S.gets (view envRequestableRoles)
        let nick = view #username member

        if | isJust . Vec.find (view #id role ==) . view #roles $ member ->
            info @Text [fmt|Member {nick} already has role {roleName}|]

           | view #name role `Set.member` available -> do
            let user = view #user ctxt
            res <- C.invoke $ C.AddGuildMemberRole guild user role
            debug @Text $ show res

            -- TODO: Guard this printout behind success response
            --       calamity currently has a bug where invocations of requests
            --       that return () always return Left, even on success.
            void . C.tell @Text ctxt $ [fmt|Assign role `{roleName}` to {nick}|]

           | otherwise -> pass

-- | Removes a (requestable) role from the user.
leaveRole :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
leaveRole ctxt roleName = case view #member ctxt of
    Nothing -> warning @Text
        $ "Could not find command invoker to be a guild member."
        <> " Check that the bot has privileged intents enabled."
    Just member -> do
        Just guild <- pure . view #guild $ ctxt
        Just role <- lookupRole guild roleName
        available <- S.gets (view envRequestableRoles)
        let nick = view #username member

        when (view #name role `Set.member` available) $ do
            let user = view #user ctxt
            _res <- C.invoke $ C.RemoveGuildMemberRole guild user role

            -- TODO: Guard this printout behind success response, see rolerequest
            void . C.tell @Text ctxt $ [fmt|Remove role `{roleName}` from {nick}|]

-- | Make the given role requestable with 'rolerequest'.
makeRequestable :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
makeRequestable ctxt roleName = do
    available <- S.gets (view envRequestableRoles)
    Just guild <- pure . view #guild $ ctxt

    lookupRole guild roleName >>= \case
        Nothing -> void $ C.tell @Text ctxt [fmt|Role `{roleName}` does not exist|]
        Just _roles
            | toLazy roleName `Set.member` available ->
                void $ C.tell @Text ctxt [fmt|Role `{roleName}` is already requestable|]
            | otherwise -> do
                S.modify' $ over envRequestableRoles (Set.insert $ toLazy roleName)
                newAvailable <- S.gets (view envRequestableRoles)
                void . C.tell ctxt $ "Made role `" <> roleName <> "` requestable"
                info @Text $ "Requestable roles are now: " <> show newAvailable

-- | Revoke requestable status of the given role.
revokeRequestable :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
revokeRequestable ctxt roleName = do
    available <- S.gets (view envRequestableRoles)
    if toLazy roleName `Set.member` available
        then do
            S.modify' $ over envRequestableRoles (Set.delete $ toLazy roleName)
            availableAfter <- S.gets (view envRequestableRoles)
            void . C.tell ctxt $ "Removed `" <> roleName <> "` from requestable roles"
            info @Text $ "Requestable roles are now: " <> show availableAfter
        else info @Text $ "Tried to revoke already non-requestable role: " <> roleName

-- | List all roles that can be requested by users.
listRequestable :: forall r . (BotC r, Members '[Fail, State Env] r) =>
    FullContext -> Sem r ()
listRequestable ctxt = do
    available <- S.gets (view envRequestableRoles)
    info @Text $ "Currently requestable roles are: " <> show available
    void $ C.tell ctxt
        $ "\nCurrently requestable roles: \n"
        <> (L.intercalate "\n" . map (flip L.snoc '`' . L.cons '`') . toList) available

--
-- * Help texts
--

rolesHelp :: L.Text
rolesHelp = [fmt|\
Commands related to requesting and managing roles.

Show help for a child command with `>>=help roles COMMAND`|]

rolerequestHelp :: L.Text
rolerequestHelp = [fmt|\
Request to be assigned a role. Show the list of requestable roles with \
`>>=roles list-requestable`.

Example:
`>>=roles request DV2021`|]

leaveRoleHelp :: L.Text
leaveRoleHelp = [fmt|\
Remove a (requestable) role from yourself. Show the list of requestable roles \
with `>>=roles list-requestable`.

Example:
`>>=roles leave DV2019`|]

listRequestableHelp :: L.Text
listRequestableHelp = [fmt|\
Show the list of requestable roles.

Example:
`>>=roles list-requestable`|]

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
