module Bot
       ( eventHandler
       ) where

import Discord (DiscordHandler)
import Discord.Types (Event (..), Message)

import Calls.Echo (cmdEcho)
import Commands (Cmd (..), cmdFromMessage)


eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate msg) = whenJust (cmdFromMessage msg) (execCmd msg)
eventHandler _ = pass

execCmd :: Message -> Cmd -> DiscordHandler ()
execCmd msg cmd = case cmd of
    CmdEcho -> do
        res <- runReader cmdEcho msg
        whenLeft_ res print
    _ -> do
        -- TODO: Behavior for this (impossible) case?
        putTextLn "execCmd: unrecognized command"
        putTextLn "pls fix."
