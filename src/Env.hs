module Env
       ( Shaun
       , Env
       , initEnv
       , getMsg
       , setMsg
       ) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), decodeFileStrict')
import Discord (DiscordHandler)
import Discord.Types (Message)
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig))
import System.FilePath ((</>))
import System.IO.Error (catchIOError, IOError)


-- * Bot configuration

data Config = Config
    { cfgRequestableRoles :: [Text]
    }

instance FromJSON Config where
    parseJSON = withObject "Config" $ \ value -> Config
        <$> value .: "requestableRoles"

defaultConfig :: Config
defaultConfig = Config
    { cfgRequestableRoles = []
    }

getConfigLocation :: IO FilePath
getConfigLocation = getXdgDirectory XdgConfig "shaunwhite" <&> (</> "config")

getConfig :: IO Config
getConfig = do
    cfgLocation <- getConfigLocation
    putTextLn $ "Reading config file from `" <> fromString cfgLocation <> "`"
    mbyCfg <- catchIOError (decodeFileStrict' cfgLocation) handleRead
    case mbyCfg of
        Just c -> return c
        Nothing -> do
            putTextLn "Failed to read config file, using default config."
            return defaultConfig
  where
    handleRead :: IOError -> IO (Maybe Config)
    handleRead err = do
        print err
        return Nothing

-- * Environment

type Shaun = ReaderT Env

data Env = Env
    { envCmdEnv :: !(TVar CmdEnv)
    }

initEnv :: IO Env
initEnv = do
    -- Read the config file.
    cfg <- getConfig

    -- Initialize the (mutable) runtime environment.
    cmdEnv <- newTVarIO $ CmdEnv
        { cmdEnvMessage          = Nothing
        , cmdEnvRequestableRoles = cfgRequestableRoles cfg
        }

    return $ Env
        { envCmdEnv = cmdEnv
        }

-- * Environment while running the bot

data CmdEnv = CmdEnv
    { -- | The message that triggered the current command.
      cmdEnvMessage          :: !(Maybe Message)
      -- | Names of roles that can be requested by any user.
    , cmdEnvRequestableRoles :: ![Text]
    }

-- * Convenient functions

getMsg :: Shaun DiscordHandler (Maybe Message)
getMsg = do
    tCmdEnv <- envCmdEnv <$> ask
    cmdEnvMessage <$> liftIO (readTVarIO tCmdEnv)

setMsg :: Message -> Shaun DiscordHandler ()
setMsg msg = do
    tCmdEnv <- envCmdEnv <$> ask
    liftIO $ atomically $ modifyTVar' tCmdEnv set
  where
    set :: CmdEnv -> CmdEnv
    set cmdEnv = cmdEnv { cmdEnvMessage = Just msg }
