module Rolerequest (
    rolerequest,
    ) where

import CustomPrelude

import Calamity (Role)
import Calamity qualified as C
import Calamity.Commands.Context (FullContext)
import Control.Lens
import Polysemy (Members)
import Polysemy qualified as P
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import Polysemy.State qualified as S

import Env (Env, envAvailRoles)


{- | Give the user issuing the bot command the role with the corresponding name.
The request is only granted if the role is in the explicit list of requestable
roles.
-}
rolerequest :: forall r . (C.BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> P.Sem r ()
rolerequest ctxt roleTxt = do
    Just guild <- pure . view #guild $ ctxt
    Right roles <- C.invoke (C.GetGuildRoles guild)
    Just role <- pure $ roleFromName roleTxt roles

    available <- S.gets (view envAvailRoles)
    when (view #name role `elem` available)
        $ void . C.invoke $ C.AddGuildMemberRole guild (ctxt ^. #user) role
  where
    roleFromName :: Text -> [Role] -> Maybe Role
    roleFromName name = find ((== name) . toStrict . view #name)
