{- |
Echoes the parameter of the command message.
-}
module Calls.Echo
       ( cmdEcho
       ) where

import Discord (DiscordHandler, RestCallErrorCode, restCall)
import Discord.Types (ChannelId, Message (..))
import Discord.Requests (ChannelRequest (CreateMessage))

import Commands (stripCommand)


cmdEcho :: Message -> DiscordHandler (Either RestCallErrorCode Message)
cmdEcho msg = restCall $ CreateMessage channel txt
  where
    channel :: ChannelId
    channel = messageChannel msg

    txt :: Text
    txt = stripCommand $ messageText msg
