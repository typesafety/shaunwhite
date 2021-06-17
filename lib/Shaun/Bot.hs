module Shaun.Bot
       ( eventHandler
       ) where

import Relude

import Discord (DiscordHandler)
import Discord.Types (Event (MessageCreate))

import Shaun.Calls.Echo (cmdEcho)
import Shaun.Calls.Help (cmdHelp)
import Shaun.Calls.RoleRequest
       ( cmdRoleRequestAdd
       , cmdRoleRequestDel
       , cmdRoleRequestList
       , cmdRoleRequestReq
       )
import Shaun.Commands (Cmd (..), cmdFromMessage, isCommandForBot)
import qualified Shaun.Env as Env

-- * Event handler and command execution

eventHandler :: Env.Env -> Event -> DiscordHandler ()
eventHandler env (MessageCreate msg) =
    whenJust (guarded isCommandForBot msg *> cmdFromMessage msg) run
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

    -- Roles
    CmdRoleRequestAdd roles -> do
        res <- cmdRoleRequestAdd roles
        either print print res
    CmdRoleRequestDel roles -> do
        res <- cmdRoleRequestDel roles
        either print print res
    CmdRoleRequestList -> do
        res <- cmdRoleRequestList
        either print print res
    CmdRoleRequestReq roles -> do
        res <- cmdRoleRequestReq roles
        either print print res

    -- Config
    CmdCfgWrite -> do
        -- TODO: make this command admin-only (p l s)
        env <- ask
        liftIO $ Env.writeConfig env
