{- |
Echoes the parameter of the command message.
-}
module Calls.Echo
       ( cmdEcho
       ) where

import Relude

import Discord (DiscordHandler, RestCallErrorCode, restCall)
import Discord.Types (ChannelId, Message (..))
import Discord.Requests (ChannelRequest (CreateMessage))

import qualified Env (Shaun, getMsg)


cmdEcho :: Text -> Env.Shaun DiscordHandler (Either RestCallErrorCode Message)
cmdEcho txt = do
    Just msg <- Env.getMsg
    lift $ performCall msg
  where
    performCall :: Message -> DiscordHandler (Either RestCallErrorCode Message)
    performCall triggerMsg = restCall $ CreateMessage channel txt
      where
        channel :: ChannelId
        channel = messageChannel triggerMsg
