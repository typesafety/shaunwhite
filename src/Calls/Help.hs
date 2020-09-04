{- |
Sends a DM to the user with instructions for using the bot.
-}
module Calls.Help
       ( cmdHelp
       ) where

import Discord (DiscordHandler, RestCallErrorCode, restCall)
import Discord.Types (Message (..), Channel (..), User (..))
import Discord.Requests (UserRequest (CreateDM), ChannelRequest (CreateMessage))

import Commands (Exec, CmdEnv (..))


cmdHelp :: Exec DiscordHandler (Either RestCallErrorCode Message)
cmdHelp = ask <&> cmdEnvMessage >>= lift . performCall
  where
    performCall :: Message -> DiscordHandler (Either RestCallErrorCode Message)
    performCall triggerMsg = do
        Right dmChan <- restCall $ reqDmChannel triggerMsg
        restCall $ reqCreateMsg dmChan
      where
        reqDmChannel :: Message -> UserRequest Channel
        reqDmChannel = CreateDM . userId . messageAuthor

        reqCreateMsg :: Channel -> ChannelRequest Message
        reqCreateMsg = flip CreateMessage helpText . channelId

        helpText :: Text
        helpText = "The help command is not yet implemeted :shrug:, no help 4 u lol"
