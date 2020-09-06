module Bot
       ( eventHandler
       ) where

import Discord (DiscordHandler)
import Discord.Types (Event (MessageCreate))

import Calls.Echo (cmdEcho)
import Calls.Help (cmdHelp)
import Commands (Cmd (..), cmdFromMessage, isBotCommand)
import Env (Env, Shaun, setMsg)


-- * Event handler and command execution

eventHandler :: Env -> Event -> DiscordHandler ()
eventHandler env (MessageCreate msg) =
    whenJust (guarded isBotCommand msg *> cmdFromMessage msg) run
  where
    run :: Cmd -> DiscordHandler ()
    run cmd = runReaderT (setMsg msg >> execCmd cmd) env

eventHandler _ _ = pass

execCmd :: Cmd -> Shaun DiscordHandler ()
execCmd cmd = case cmd of
    CmdEcho txt -> do
        res <- cmdEcho txt
        either print print res
    CmdHelp -> do
        res <- cmdHelp
        either print print res
    -- CmdRoleAddRequestable roles -> do
    --     res <- cmdRoleRequestAdd roles
    _ -> do
        -- TODO: Behavior for this (impossible) case?
        putTextLn $ "execCmd: command not yet implemented: `" <> show cmd <> "`"
        putTextLn "pls fix."
