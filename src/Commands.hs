module Commands
       ( -- * ADT representing bot commands.
         Cmd (..)

         -- * Parser from Message" to "Cmd".
       , cmdFromMessage

         -- * Utilities.
       , botPrefix
       , isBotCommand
       , stripCommand
       ) where

import Discord.Types (Message (..), userIsBot)

import qualified Data.Text as T


data Cmd
    = CmdHelp
    | CmdEcho Text

    -- Commands for role requests.
    | CmdRoleRequestAdd [Text]  -- Set roles as requestable.
    | CmdRoleRequestDel [Text]  -- Make roles non-requestable.
    | CmdRoleRequestReq [Text]  -- Request roles.
    deriving (Show)

cmdFromMessage :: Message -> Maybe Cmd
cmdFromMessage msg = do
    let msgTxt = messageText msg
    Just (command : args) <- pure $ words <$> T.stripPrefix botPrefix msgTxt
    case command of
        "help"        -> Just CmdHelp
        "echo"        -> Just $ CmdEcho (stripCommand command msgTxt)
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

stripCommand
    :: Text  -- ^ The command (e.g. "echo")
    -> Text  -- ^ The full text of the message triggering the command (e.g. ">>=echo hey")
    -> Text  -- ^ The full text, stripped of the prefix and command (e.g. "hey")
stripCommand command = T.stripStart . T.drop (T.length command) . snd . T.breakOn command
