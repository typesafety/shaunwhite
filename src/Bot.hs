module Bot
       ( eventHandler
       , Cfg (..)
       ) where


import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import Discord (DiscordHandler)
import Discord.Types (Event (MessageCreate))

import Calls.Echo (cmdEcho)
import Calls.Help (cmdHelp)
import Commands (CmdEnv (..), Exec, Cmd (..), cmdFromMessage, isBotCommand)


data Cfg = Cfg
    { cfgRequestableRoles :: [Text]
    }

instance FromJSON Cfg where
    parseJSON = withObject "Cfg" $ \ value -> Cfg
        <$> value .: "requestableRoles"

eventHandler :: Cfg -> Event -> DiscordHandler ()
eventHandler cfg (MessageCreate msg) =
    whenJust (guarded isBotCommand msg *> cmdFromMessage msg) evalCmd
  where
    evalCmd :: Cmd -> DiscordHandler ()
    evalCmd command = evalStateT (execCmd command) environment

    environment :: CmdEnv
    environment = CmdEnv
        { cmdEnvMessage          = msg
        , cmdEnvRequestableRoles = cfgRequestableRoles cfg
        }
eventHandler _ _ = pass

execCmd :: Cmd -> Exec DiscordHandler ()
execCmd cmd = case cmd of
    CmdEcho -> do
        res <- cmdEcho
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
