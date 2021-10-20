{-# LANGUAGE QuasiQuotes #-}

module Rolerequest (
    listRequestable,
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
import Data.Text.Lazy qualified as L
import Data.Vector.Unboxing qualified as Vec
import DiPolysemy (debug, info, warning)
import Polysemy (Member, Members, Sem)
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import Polysemy.State qualified as S
import PyF (fmt)

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
            void . C.tell @Text ctxt $ [fmt|Assign role `{roleName}` to {nick}|]

           | otherwise -> pass

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
            void . C.tell ctxt $ "Made role `" <> roleName <> "` requestable"
            info @Text $ "Requestable roles are now: " <> show available

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
        $ "Currently requestable roles: \n"
        <> L.intercalate "\n" (toList available)

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
