{-# LANGUAGE QuasiQuotes #-}

module Roles (
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
import Data.Text qualified as T
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
registerRolesCommands :: forall r .
    ( BotC r
    , Members '[State Env, State [C.Command FullContext]] r
    )
    => Sem (C.DSLState FullContext r) ()
registerRolesCommands = C.help (const rolesHelp) $ C.group' "roles" $ do
    --
    -- Normal user commands
    --

    -- Request to be assigned a role from the list of requestable roles.
    void $ C.help (const rolerequestHelp) $ C.command @'[Text] "request" rolerequest

    -- Remove an assigned (requestable) role from yourself.
    void $ C.help (const leaveRoleHelp) $ C.command @'[Text] "leave" leaveRole

    -- Show the list of requestable roles.
    void
        $ C.help (const listRequestableHelp)
        $ C.command @'[] "list-requestable" listRequestable

    --
    -- Admin commands (only register with `registerAdminCmd`)
    --

    -- Add a role as requestable.
    registerAdminCmd . C.hide
        $ C.help (const "Make a command requestable")
        $ C.command @'[Text] "make-requestable" makeRequestable

    -- Revoke a role as requestable.
    registerAdminCmd . C.hide
        $ C.help (const "Make a command non-requestable")
        $ C.command @'[Text] "revoke-requestable" revokeRequestable

    -- Give a (requestable) role to everyone.
    registerAdminCmd . C.hide
        $ C.help (const "Give a role to everyone in the server")
        $ C.command @'[Text] "give-all" roleToAll

    -- Remove a role from everyone.
    registerAdminCmd . C.hide
        $ C.help (const "Remove a role from everyone in the server")
        $ C.command @'[Text] "remove-all" roleRemoveFromAll

-- | Give a (requestable) role to everyone on the server.
roleToAll :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
roleToAll ctxt roleName = do
    requestables <- S.gets (view envRequestableRoles)
    if (roleName `Set.member` requestables)
        then do
            Just guild <- pure . view #guild $ ctxt
            Just role <- lookupRole guild roleName
            void . C.tell ctxt $ "Giving EVERYONE role: `" <> roleName <> "`"
            onAllMembers ctxt (\g m -> C.invoke $ C.AddGuildMemberRole g m role)
        else void . C.tell @Text ctxt $ [fmt|`{roleName}` is not a requestable role|]

-- | Remove a role from everyone on the server.
roleRemoveFromAll :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
roleRemoveFromAll ctxt roleName = do
    Just guild <- pure . view #guild $ ctxt
    Just role <- lookupRole guild roleName
    void . C.tell ctxt $ "Removing the following role from EVERYONE: `" <> roleName <> "`"
    onAllMembers ctxt (\g m -> C.invoke $ C.RemoveGuildMemberRole g m role)

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

            -- TODO: Guard this printout behind success response
            debug @Text $ "<<<RESULT>>> " <> show res
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
            | roleName `Set.member` available ->
                void $ C.tell @Text ctxt [fmt|Role `{roleName}` is already requestable|]
            | otherwise -> do
                S.modify' $ over envRequestableRoles (Set.insert roleName)
                newAvailable <- S.gets (view envRequestableRoles)
                void . C.tell ctxt $ "Made role `" <> roleName <> "` requestable"
                info @Text $ "Requestable roles are now: " <> show newAvailable

-- | Revoke requestable status of the given role.
revokeRequestable :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> Sem r ()
revokeRequestable ctxt roleName = do
    available <- S.gets (view envRequestableRoles)
    if roleName `Set.member` available
        then do
            S.modify' $ over envRequestableRoles (Set.delete roleName)
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
        <> (T.intercalate "\n" . map (flip T.snoc '`' . T.cons '`') . toList) available

--
-- * Helper functions
--

-- | Perform an action on all members of a guild, such as adding/removing a role.
onAllMembers :: forall r a . (BotC r, Member Fail r)
    => FullContext -> (Guild -> C.Member -> Sem r a) -> Sem r ()
onAllMembers ctxt f = do
    Just g <- pure $ view #guild ctxt
    Right members <- C.invoke
        $ C.ListGuildMembers g (C.ListMembersOptions (Just 999) Nothing)
    mapM_ (f g) members


-- | Return the role of the given name if it exists in the guild, or Nothing otherwise.
lookupRole :: (BotC r, Member Fail r) => Guild -> Text -> Sem r (Maybe Role)
lookupRole guild roleName = do
    Right roles <- C.invoke (C.GetGuildRoles guild)
    pure $ roleFromName roleName roles
  where
    roleFromName :: Text -> [Role] -> Maybe Role
    roleFromName name = find ((== name) . view #name)

--
-- * Help texts
--

rolesHelp :: Text
rolesHelp = [fmt|\
Commands related to requesting and managing roles.

Show help for a child command with `>>=help roles COMMAND`|]

rolerequestHelp :: Text
rolerequestHelp = [fmt|\
Request to be assigned a role. Show the list of requestable roles with \
`>>=roles list-requestable`.

Example:
`>>=roles request DV2021`|]

leaveRoleHelp :: Text
leaveRoleHelp = [fmt|\
Remove a (requestable) role from yourself. Show the list of requestable roles \
with `>>=roles list-requestable`.

Example:
`>>=roles leave DV2019`|]

listRequestableHelp :: Text
listRequestableHelp = [fmt|\
Show the list of requestable roles.

Example:
`>>=roles list-requestable`|]
