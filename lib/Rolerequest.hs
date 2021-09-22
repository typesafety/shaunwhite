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

import Config (Env, envAvailRoles)


rolerequest :: forall r . (C.BotC r, Members '[Fail, State Env] r)
    => FullContext -> Text -> P.Sem r ()
rolerequest ctxt roleTxt = do
    Just guild <- pure . view #guild $ ctxt
    Right roles <- C.invoke (C.GetGuildRoles guild)
    Just role <- pure $ roleFromName roleTxt roles

    -- TODO: Better code style
    available <- S.gets (view envAvailRoles)
    when (view #name role `elem` available) $ do
        let guildReq = C.AddGuildMemberRole guild (ctxt ^. #user) role
        void . C.invoke $ guildReq
  where
    roleFromName :: Text -> [Role] -> Maybe Role
    roleFromName name = find ((== name) . toStrict . view #name)
