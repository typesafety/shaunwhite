{- |
Sends a DM to the user with instructions for using the bot.
-}
module Shaun.Calls.Help
       ( cmdHelp
       ) where

import Relude

import Discord (DiscordHandler, RestCallErrorCode, restCall)
import Discord.Types (Message (..), Channel (..), User (..))
import Discord.Requests (UserRequest (CreateDM), ChannelRequest (CreateMessage))

import Shaun.Env (Shaun, getMsg)


cmdHelp :: Shaun DiscordHandler (Either RestCallErrorCode Message)
cmdHelp = do
    Just msg <- getMsg
    lift $ performCall msg
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
