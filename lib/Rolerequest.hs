module Rolerequest (
    makeRequestable,
    rolerequest,
    ) where

import CustomPrelude

import Calamity (BotC, Guild, Role)
import Calamity qualified as C
import Calamity.Commands.Context (FullContext)
import Control.Lens
import DiPolysemy (info, warning)
import Polysemy (Member, Members)
import Polysemy qualified as P
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import Polysemy.State qualified as S

import Env (Env, envAvailRoles)


{- | Give the user issuing the bot command the role with the corresponding name.
The request is only granted if the role is in the explicit list of requestable
roles.
-}
rolerequest :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> P.Sem r ()
rolerequest ctxt roleName = do
    Just guild <- pure . view #guild $ ctxt
    Just role <- lookupRole guild roleName

    available <- S.gets . view $ envAvailRoles
    when (view #name role `elem` available)
        $ void . C.invoke $ C.AddGuildMemberRole guild (ctxt ^. #user) role

-- | Make the given role requestable with 'rolerequest'.
makeRequestable :: forall r . (BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> P.Sem r ()
makeRequestable ctxt roleName = do
    Just guild <- pure . view #guild $ ctxt
    lookupRole guild roleName >>= \case
        Nothing -> warning @Text $
            "Tried making non-existing role requestable: " <> roleName
        Just _role -> do
            S.modify' $ over envAvailRoles (toLazy roleName :)
            available <- S.gets . view $ envAvailRoles
            info @Text $ "Made role `" <> roleName <> "` requestable"
            info @Text $ "Requestable roles are now: " <> show available

-- | Return the role of the given name if it exists in the guild, or Nothing otherwise.
lookupRole :: (BotC r, Member Fail r) => Guild -> Text -> P.Sem r (Maybe Role)
lookupRole guild roleName = do
    Right roles <- C.invoke (C.GetGuildRoles guild)
    pure $ roleFromName roleName roles
  where
    roleFromName :: Text -> [Role] -> Maybe Role
    roleFromName name = find ((== name) . toStrict . view #name)

