module Bot
       ( eventHandler
       ) where

import Discord (DiscordHandler)
import Discord.Types (Event (MessageCreate))

import Calls.Echo (cmdEcho)
import Calls.Help (cmdHelp)
import Calls.RoleRequest
       ( cmdRoleRequestAdd
       , cmdRoleRequestDel
       , cmdRoleRequestList
       )
import Commands (Cmd (..), cmdFromMessage, isBotCommand)

import qualified Env


-- * Event handler and command execution

eventHandler :: Env.Env -> Event -> DiscordHandler ()
eventHandler env (MessageCreate msg) =
    whenJust (guarded isBotCommand msg *> cmdFromMessage msg) run
  where
    run :: Cmd -> DiscordHandler ()
    run cmd = do
        runReaderT (Env.setMsg msg >> execCmd cmd) env
        let tCmdEnv = Env.envCmdEnv env
        cmdEnv <- liftIO $ readTVarIO tCmdEnv
        liftIO $ print cmdEnv

eventHandler _ _ = pass

execCmd :: Cmd -> Env.Shaun DiscordHandler ()
execCmd cmd = case cmd of
    CmdEcho txt -> do
        res <- cmdEcho txt
        either print print res
    CmdHelp -> do
        res <- cmdHelp
        either print print res
    CmdRoleRequestAdd roles -> do
        res <- cmdRoleRequestAdd roles
        either print print res
    CmdRoleRequestDel roles -> do
        res <- cmdRoleRequestDel roles
        either print print res
    CmdRoleRequestList -> do
        res <- cmdRoleRequestList
        either print print res
    _ -> do
        -- TODO: Behavior for this (impossible) case?
        putTextLn $ "execCmd: command not yet implemented: `" <> show cmd <> "`"
        putTextLn "pls fix."
