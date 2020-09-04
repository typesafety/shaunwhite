{- |
Echoes the parameter of the command message.
-}
module Calls.Echo
       ( cmdEcho
       ) where

import Discord (DiscordHandler, RestCallErrorCode, restCall)
import Discord.Types (ChannelId, Message (..))
import Discord.Requests (ChannelRequest (CreateMessage))

import Commands (Exec, CmdEnv (..), stripCommand)


cmdEcho :: Exec DiscordHandler (Either RestCallErrorCode Message)
cmdEcho = gets cmdEnvMessage >>= lift . performCall
  where
    performCall :: Message -> DiscordHandler (Either RestCallErrorCode Message)
    performCall triggerMsg = restCall $ CreateMessage channel txt
      where
        channel :: ChannelId
        channel = messageChannel triggerMsg

        txt :: Text
        txt = stripCommand $ messageText triggerMsg
