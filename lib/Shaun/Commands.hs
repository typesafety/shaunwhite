module Shaun.Commands
       ( -- * ADT representing bot commands.
         Cmd (..)

         -- * Parser from Message" to "Cmd".
       , cmdFromMessage

         -- * Utilities.
       , botPrefix
       , isCommandForBot
       , stripCommand
       ) where

import Relude

import Discord.Types (Message (..), userIsBot)

import qualified Data.Text as T


data Cmd
    = CmdHelp       -- ^ TODO
    | CmdEcho Text  -- ^ Output the provided string.

    -- Commands for role requests.
    | CmdRoleRequestAdd [Text]  -- ^ Set roles as requestable.
    | CmdRoleRequestDel [Text]  -- ^ Make roles non-requestable.
    | CmdRoleRequestReq [Text]  -- ^ Request roles.
    | CmdRoleRequestList        -- ^ Output requestable roles.

    -- | Write the config file to disk.
    | CmdCfgWrite
    deriving (Show)

-- | Attempt to parse a 'Message' into a 'Cmd', whitespace-insensitive.
-- TODO: Better parsing implementation, this thing below is fragile af
cmdFromMessage :: Message -> Maybe Cmd
cmdFromMessage msg = do
    let msgTxt = messageText msg
    Just (command : args) <- pure $ words <$> T.stripPrefix botPrefix msgTxt
    case command of
        "help"        -> Just CmdHelp
        "echo"        -> Just $ CmdEcho (stripCommand command msgTxt)
        "rolerequest" -> parseRoleRequest args
        "config"      -> parseCfg args
        _             -> Nothing
  where
    parseCfg :: [Text] -> Maybe Cmd
    parseCfg ["write"] = Just CmdCfgWrite
    parseCfg _         = Nothing

    parseRoleRequest :: [Text] -> Maybe Cmd
    parseRoleRequest [] = Nothing
    parseRoleRequest args@(subCmd : roles) = Just $ case subCmd of
        "add"  -> CmdRoleRequestAdd roles
        "del"  -> CmdRoleRequestDel roles
        "list" -> CmdRoleRequestList
        "--"   -> CmdRoleRequestReq roles
        _      -> CmdRoleRequestReq args

-- * Utilities.

botPrefix :: Text
botPrefix = ">>="

-- | Determine whether the bot should care about the message at all.
--
-- Conditions:
--
-- * Message was not sent by a bot.
-- * The text of the message is prefixed by the text given by 'botPrefix'.
isCommandForBot :: Message -> Bool
isCommandForBot msg = all ($ msg) conditions
    where
    conditions :: [Message -> Bool]
    conditions =
        [ not . userIsBot . messageAuthor
        , (botPrefix `T.isPrefixOf`) . messageText
        ]

-- | Clean a command from the bot prefix and extra whitespace.
-- >>> stripCommand "echo" ">>= echo hey"
-- "hey"
stripCommand
    :: Text  -- ^ The command (e.g. "echo")
    -> Text  -- ^ The full text of the message triggering the command (e.g. ">>=echo hey")
    -> Text  -- ^ The full text, stripped of the prefix and command (e.g. "hey")
stripCommand command = T.stripStart . T.drop (T.length command) . snd . T.breakOn command
