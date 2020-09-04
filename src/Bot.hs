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
eventHandler cfg (MessageCreate msg)
    | isBotCommand msg = runReaderT handleMsg environment
  where
    environment :: CmdEnv
    environment = CmdEnv
        { cmdEnvMessage          = msg
        , cmdEnvRequestableRoles = cfgRequestableRoles cfg
        }

    handleMsg :: Exec DiscordHandler ()
    handleMsg = do
        Just cmd <- cmdFromMessage
        execCmd cmd
eventHandler _ _ = pass

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
