module Bot
       ( eventHandler
       ) where

import Discord (DiscordHandler)
import Discord.Types (Event (MessageCreate))

import Calls.Echo (cmdEcho)
import Calls.Help (cmdHelp)
import Commands (Env (..), Exec, Cmd (..), cmdFromMessage, isBotCommand)


eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate msg)
    | isBotCommand msg = runReaderT handleMsg (Env msg)
  where
    handleMsg :: Exec DiscordHandler ()
    handleMsg = do
        Just cmd <- cmdFromMessage
        execCmd cmd
eventHandler _ = pass

execCmd :: Cmd -> Exec DiscordHandler ()
execCmd cmd = case cmd of
    CmdEcho -> do
        res <- cmdEcho
        either print print res
    CmdHelp -> do
        res <- cmdHelp
        either print print res
    _ -> do
        -- TODO: Behavior for this (impossible) case?
        putTextLn $ "execCmd: command not yet implemented: `" <> show cmd <> "`"
        putTextLn "pls fix."
