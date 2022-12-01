-- | Module for handlers and commands related to new members joining the server.

module Welcome where

import CustomPrelude

import Calamity (BotC, Guild, Role)
import Calamity.Types.Model.Guild.Member (Member (..))
-- import Calamity qualified as C
-- import Calamity.Commands qualified as C
-- import Calamity.Commands.Context (FullContext)
import Calamity qualified as C
import Calamity.Commands qualified as C
import Control.Lens (view)
-- import Data.Set qualified as Set
-- import Data.Text qualified as T
-- import Data.Vector.Unboxing qualified as Vec
import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)
import Polysemy.State (State)
import Polysemy.State qualified as S
import PyF (fmt)
import Env (Env, envWelcomeRoles)



handleMemberJoined :: forall r . (BotC r, Members '[State Env, Fail] r) => Member -> Sem r ()
handleMemberJoined member = do
    let gid = view #guildID member
    guild <- C.invoke $ C.GetGuild gid

    -- guild :: _ <- C.invoke $ C.GetGuild gid
    -- Look up the roles.
    -- let roleNames = S.gets (view envWelcomeRoles)
    -- Just (roles :: _a) <- mapM (lookupRole guild) roleNames

    -- let nick = view #username member
    -- info @Text [fmt|Member {nick} joined the server; assigning roles.|]
    pure ()
    
-- | Return the role of the given name if it exists in the guild, or Nothing otherwise.
lookupRole :: (BotC r, Members '[Fail] r) => Guild -> Text -> Sem r (Maybe Role)
lookupRole guild roleName = do
    Right roles <- C.invoke (C.GetGuildRoles guild)
    pure $ roleFromName roleName roles
  where
    roleFromName :: Text -> [Role] -> Maybe Role
    roleFromName name = find ((== name) . view #name)


    -- Just guild <- pure . view #guild $ ctxt
    -- Just role <- lookupRole guild roleName
    -- available <- S.gets (view envRequestableRoles)
    -- let nick = view #username member

    -- if | isJust . Vec.find (view #id role ==) . view #roles $ member ->
    --     info @Text [fmt|Member {nick} already has role {roleName}|]

    --    | view #name role `Set.member` available -> do
    --     let user = view #user ctxt
    --     res <- C.invoke $ C.AddGuildMemberRole guild user role

    --     -- TODO: Guard this printout behind success response
    --     debug @Text $ "<<<RESULT>>> " <> show res
    --     void . C.tell @Text ctxt $ [fmt|Assign role `{roleName}` to {nick}|]

    --    | otherwise -> pass


