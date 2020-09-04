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

import Discord.Types

import qualified Data.Text as T


-- For bot commands, holds the message that triggered the command.
-- Used for getting information about the user, the guild, etc.
type Exec = StateT CmdEnv

data CmdEnv = CmdEnv
    { cmdEnvMessage          :: Message
    , cmdEnvRequestableRoles :: [Text]
    }

data Cmd
    = CmdHelp
    | CmdEcho

    -- Commands for role requests.
    | CmdRoleRequestAdd [Text] -- Set roles as requestable.
    | CmdRoleRequestDel [Text] -- Make roles non-requestable.
    | CmdRoleRequestReq [Text] -- Request roles.
    deriving (Show)

cmdFromMessage :: Message -> Maybe Cmd
cmdFromMessage msg = do
    Just (command : args) <- pure $ words <$> T.stripPrefix botPrefix (messageText msg)
    case command of
        "help"        -> Just CmdHelp
        "echo"        -> Just CmdEcho
        "rolerequest" -> parseRoleRequest args
        _             -> Nothing
  where
    parseRoleRequest :: [Text] -> Maybe Cmd
    parseRoleRequest [] = Nothing
    parseRoleRequest args@(subCmd : roles) = case subCmd of
        "add" -> Just $ CmdRoleRequestAdd roles
        "del" -> Just $ CmdRoleRequestDel roles
        "--"  -> Just $ CmdRoleRequestReq roles
        _     -> Just $ CmdRoleRequestReq args

-- * Utilities.

botPrefix :: Text
botPrefix = ">>="

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
