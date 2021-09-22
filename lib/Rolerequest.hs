module Rolerequest (
    rolerequest,
    ) where

import CustomPrelude

import Calamity qualified as C
import Calamity (Role)
import Calamity.Commands.Context (FullContext)
import Control.Lens
import Polysemy (Member)
import Polysemy qualified as P
import Polysemy.Fail (Fail)


rolerequest :: forall r . (C.BotC r, Member Fail r)
    => FullContext -> Text -> P.Sem r ()
rolerequest ctxt roleTxt = do
    Just guild <- pure . view #guild $ ctxt
    Right roles <- C.invoke (C.GetGuildRoles guild)
    Just role <- pure $ roleFromName roleTxt roles

    let guildReq = C.AddGuildMemberRole guild (ctxt ^. #user) role
    void . C.invoke $ guildReq
  where
    roleFromName :: Text -> [Role] -> Maybe Role
    roleFromName name = find ((== name) . toStrict . view #name)
