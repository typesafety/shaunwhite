module Auth (
    checkIsAdmin,
    registerAdminCmd,
    ) where

import CustomPrelude

import Calamity qualified as C
import Calamity.Commands.Context (FullContext)
import Calamity.Commands.Dsl (requires)
import Calamity.Commands.Types
  ( Check,
    Command,
    DSLState,
  )
import CalamityCommands.Check (buildCheck)
import Control.Lens
import Data.Flags (containsAll)
import Data.Text.Lazy qualified as L
import Polysemy (Member, Sem)
import Polysemy.Fail (Fail, runFail)


-- | Register a command that requires an admin to issue it.
registerAdminCmd :: (C.BotC r, FullContext ~ c) =>
    Sem (DSLState c r) (Command c) ->
    Sem (DSLState c r) ()
registerAdminCmd cmd = do
    adminCheck <- checkIsAdmin
    void $ requires [adminCheck] cmd

{- | Construct a Check for a command, requiring that the user issuing the
command is an admin of the server.
-}
checkIsAdmin :: forall r . (C.BotC r) => Sem r (Check FullContext)
checkIsAdmin = buildCheck "Require that user is admin" check
  where
    check :: FullContext -> Sem r (Maybe L.Text)
    check ctxt = runFail (getMember ctxt) >>= \case
        Left failMsg -> pure $ Just (fromString failMsg)
        Right member -> do
            userPerms <- view #guildID member `C.permissionsIn'` member
            pure $ if userPerms `containsAll` C.administrator
                then Nothing
                else Just "User must be admin"

    getMember :: (C.BotC s, Member Fail s) => FullContext -> Sem s C.Member
    getMember ctxt = do
        let user = view #user ctxt
        Just guild <- pure . view #guild $ ctxt
        Right member <- C.invoke (C.GetGuildMember guild user)
        pure member
