module Commands
       ( -- * ADT representing bot commands.
         Cmd (..)

       , Exec

         -- * Parser from Message" to "Cmd".
       , cmdFromMessage

         -- * Utilities.
       , botPrefix
       , stripCommand
       ) where

import Discord.Types

import qualified Data.Text as T


-- TODO: Better name?
type Exec = Reader Message

data Cmd
    = CmdHelp
    | CmdEcho
    | CmdRoleInvite Text
    deriving (Show)

cmdFromMessage :: Message -> Maybe Cmd
cmdFromMessage message = do
    -- Message has the correct prefix and was written by a user.
    unless (isBotCommand message) Nothing
    parseCmd message

  where
    parseCmd :: Message -> Maybe Cmd
    parseCmd msg = do
        let txt = messageText msg
        (command : _) <- words <$> T.stripPrefix botPrefix txt
        case command of
            "help" -> Just CmdHelp
            "echo" -> Just CmdEcho
            _      -> Nothing

    isBotCommand :: Message -> Bool
    isBotCommand msg = all ($ msg) conditions
      where
        conditions :: [Message -> Bool]
        conditions =
            [ not . userIsBot . messageAuthor
            , (botPrefix `T.isPrefixOf`) . messageText
            ]

-- * Utilities.

botPrefix :: Text
botPrefix = "sw:"

stripCommand :: Text -> Text
stripCommand = T.stripStart . snd . T.break (== ' ')
