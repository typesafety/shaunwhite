module Auth (
    Checked (),
    addCheckedCommands,
    checkCmd,
    checkRegularCmd,
    checkIsAdmin,
    ) where

import CustomPrelude

import Calamity qualified as C
import Calamity.Commands (addCommands)
import Calamity.Commands.Context (FullContext)
import Calamity.Commands.Dsl (requires)
import Calamity.Commands.Types
  ( Check,
    Command,
    CommandContext,
    CommandHandler,
    DSLState,
  )
import CalamityCommands.Check (buildCheck)
import CalamityCommands.Context (ConstructContext)
import CalamityCommands.ParsePrefix (ParsePrefix)
import Control.Lens
import Data.Flags (containsAll)
import Data.Text.Lazy qualified as L
import Polysemy (Members, Sem)


-- | Abstract class whose constructor should not be exported.
newtype Checked a = Checked a

-- | Register a command that requires an admin to issue it.
checkCmd :: (C.BotC r, FullContext ~ c) =>
    Sem (DSLState c r) (Command c) ->
    Checked (Sem (DSLState c r) (Command c))
checkCmd cmd = Checked $ do
    adminCheck <- checkIsAdmin
    requires [adminCheck] cmd

-- | Register a command that can be issued by anyone.
checkRegularCmd ::
    Sem (DSLState c r) (Command c) ->
    Checked (Sem (DSLState c r) (Command c))
checkRegularCmd = Checked . requires []

-- | Wrapper around addCommands for registering Checked commands.
addCheckedCommands ::
    ( C.BotC r
    , Typeable c
    , CommandContext c
    , Members '[ParsePrefix C.Message, ConstructContext C.Message c IO ()] r
    ) =>
    [Checked (Sem (DSLState c r) a)] ->
    Sem r (Sem r (), CommandHandler c, ())
addCheckedCommands cs = addCommands $ mapM_ (\(Checked c) -> c) cs

{- | Construct a Check for a command, requiring that the user issuing the
command is an admin of the server.
-}
checkIsAdmin :: forall r . (C.BotC r) => Sem r (Check FullContext)
checkIsAdmin = buildCheck "Require that user is admin" check
  where
    check :: FullContext -> Sem r (Maybe L.Text)
    check ctxt = case view #member ctxt of
        Nothing     -> pure $ Just "Command must be run in a server"
        Just member -> do
            userPerms <- view #guildID member `C.permissionsIn'` member
            pure $ if userPerms `containsAll` C.administrator
                then Nothing
                else Just "User must be admin"
