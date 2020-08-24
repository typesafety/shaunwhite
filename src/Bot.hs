module Bot
       ( eventHandler
       ) where

import Discord (DiscordHandler)
import Discord.Types (Event (MessageCreate), Message)

import Calls.Echo (cmdEcho)
import Calls.Help (cmdHelp)
import Commands (Cmd (..), cmdFromMessage)


eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate msg) = whenJust (cmdFromMessage msg) (execCmd msg)
eventHandler _ = pass

execCmd :: Message -> Cmd -> DiscordHandler ()
execCmd msg cmd = case cmd of
    CmdEcho -> do
        res <- runReader cmdEcho msg
        either print print res
    CmdHelp -> do
        res <- runReader cmdHelp msg
        either print print res
    _ -> do
        -- TODO: Behavior for this (impossible) case?
        putTextLn $ "execCmd: unrecognized command `" <> show cmd <> "`"
        putTextLn "pls fix."
