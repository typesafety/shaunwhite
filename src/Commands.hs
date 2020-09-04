module Commands
       ( -- * ADT representing bot commands.
         Cmd (..)

       , CmdEnv (..)
       , Exec

         -- * Parser from Message" to "Cmd".
       , cmdFromMessage

         -- * Utilities.
       , botPrefix
       , isBotCommand
       , stripCommand
       ) where

import Discord
import Discord.Types

import qualified Data.Text as T


-- For bot commands, holds the message that triggered the command.
-- Used for getting information about the user, the guild, etc.
type Exec = ReaderT CmdEnv

data CmdEnv = CmdEnv
    { cmdEnvMessage          :: Message
    , cmdEnvRequestableRoles :: [Text]
    }

data Cmd
    = CmdHelp
    | CmdEcho

    -- Commands for role requests.
    | CmdRoleAddRequestable Text
    | CmdRoleDelRequestable Text
    | CmdRoleRequest Text
    deriving (Show)

cmdFromMessage :: Exec DiscordHandler (Maybe Cmd)
cmdFromMessage = do
    txt <- messageText . cmdEnvMessage <$> ask
    Just (command : _) <- pure $ words <$> T.stripPrefix botPrefix txt
    case command of
        "help"        -> pure . pure $ CmdHelp
        "echo"        -> pure . pure $ CmdEcho
        -- "rolerequest" -> pure
        _             -> pure Nothing

-- * Utilities.

botPrefix :: Text
botPrefix = "sw:"

isBotCommand :: Message -> Bool
isBotCommand msg = all ($ msg) conditions
    where
    conditions :: [Message -> Bool]
    conditions =
        [ not . userIsBot . messageAuthor
        , (botPrefix `T.isPrefixOf`) . messageText
        ]

stripCommand :: Text -> Text
stripCommand = T.stripStart . snd . T.break (== ' ')
