module Bot
       ( eventHandler

       -- * Bot configuration
       , Cfg (..)
       , defaultCfg
       , getConfig
       ) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), decodeFileStrict')
import Discord (DiscordHandler)
import Discord.Types (Event (MessageCreate))
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig))
import System.FilePath ((</>))
import System.IO.Error (catchIOError, IOError)

import Calls.Echo (cmdEcho)
import Calls.Help (cmdHelp)
import Commands (CmdEnv (..), Exec, Cmd (..), cmdFromMessage, isBotCommand)


-- * Bot configuration

data Cfg = Cfg
    { cfgRequestableRoles :: [Text]
    }

instance FromJSON Cfg where
    parseJSON = withObject "Cfg" $ \ value -> Cfg
        <$> value .: "requestableRoles"

defaultCfg :: Cfg
defaultCfg = Cfg
    { cfgRequestableRoles = []
    }

getConfigLocation :: IO FilePath
getConfigLocation = getXdgDirectory XdgConfig "shaunwhite" <&> (</> "config")

getConfig :: IO Cfg
getConfig = do
    cfgLocation <- getConfigLocation
    putTextLn $ "Reading config file from `" <> fromString cfgLocation <> "`"
    mbyCfg <- catchIOError (decodeFileStrict' cfgLocation) handleRead
    case mbyCfg of
        Just c -> return c
        Nothing -> do
            putTextLn "Failed to read config file, using default config."
            return defaultCfg
  where
    handleRead :: IOError -> IO (Maybe Cfg)
    handleRead err = do
        print err
        return Nothing

-- * Event handler and command execution

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
