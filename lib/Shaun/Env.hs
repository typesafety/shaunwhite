module Shaun.Env
       ( Shaun

       , Env (..)
       , CmdEnv (..)
       , initEnv
       , getMsg
       , setMsg
       , modEnv
       , getCmdEnv
       , writeConfig
       ) where

import Relude

import Control.Monad.Extra (findM)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Discord (DiscordHandler)
import Discord.Types (Message)
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig), doesFileExist)
import System.FilePath ((</>))
import System.IO.Error (catchIOError, tryIOError, IOError)

import qualified Data.Aeson as Aeson
import qualified Data.Set as S


-- * Bot configuration

data Config = Config
    { cfgRequestableRoles :: ![Text]
    }

instance FromJSON Config where
    parseJSON = Aeson.withObject "Config" $ \ value -> Config
        <$> value .: "requestableRoles"

instance ToJSON Config where
    toJSON cfg = Aeson.object
        [ "requestableRoles" .= cfgRequestableRoles cfg
        ]

defaultConfig :: Config
defaultConfig = Config
    { cfgRequestableRoles = []
    }

getConfigLocation :: IO (Maybe FilePath)
getConfigLocation = do
    xdgDir <- getXdgDirectory XdgConfig "shaunwhite"
    let locations :: [FilePath]
        locations =
            [ xdgDir </> "config"
            ]
    findM doesFileExist locations

getConfig :: IO (Config, FilePath)
getConfig = do
    Just cfgLocation <- getConfigLocation
    putTextLn $ "Reading config file from `" <> fromString cfgLocation <> "`"
    Just cfg <- catchIOError (Aeson.decodeFileStrict' cfgLocation) handleRead
    return (cfg, cfgLocation)
  where
    handleRead :: IOError -> IO (Maybe Config)
    handleRead err = do
        print err
        return Nothing

-- | Write persistent values from the environment to the config on disk.
writeConfig :: Env -> IO ()
writeConfig env = do
    cfg <- envToConfig env
    -- For now, just print out errors if they occur.
    catchIOError (Aeson.encodeFile (envCfgPath env) cfg) print
  where
    -- Extract values that should persist from the environment.
    envToConfig :: Env -> IO Config
    envToConfig e = do
        cmdEnv <- readTVarIO $ envCmdEnv e
        return $ Config
            { cfgRequestableRoles = S.toList $ cmdEnvRequestableRoles cmdEnv
            }

-- * Environment

type Shaun = ReaderT Env

data Env = Env
    { envCmdEnv  :: !(TVar CmdEnv)
    , envCfgPath :: !FilePath
    }

initEnv :: IO Env
initEnv = do
    -- Read the config file.
    (cfg, cfgPath) <- tryIOError getConfig >>= \case
        Right x  -> return x
        Left err -> do
            print err
            putTextLn "Config error, using default config."
            return defaultCfg

    -- Initialize the (mutable) runtime environment.
    cmdEnv <- newTVarIO $ CmdEnv
        { cmdEnvMessage          = Nothing
        , cmdEnvRequestableRoles = S.fromList $ cfgRequestableRoles cfg
        }

    return $ Env
        { envCmdEnv = cmdEnv
        , envCfgPath = cfgPath
        }
  where
    defaultCfg :: (Config, FilePath)
    defaultCfg = (defaultConfig, "shaunwhite.confg")

-- * Environment while running the bot

data CmdEnv = CmdEnv
    { -- | The message that triggered the current command.
      cmdEnvMessage          :: !(Maybe Message)
      -- | Names of roles that can be requested by any user.
    , cmdEnvRequestableRoles :: !(Set Text)
    }
    deriving (Show)

-- * Convenient functions

modEnv :: (CmdEnv -> CmdEnv) -> Shaun DiscordHandler ()
modEnv f = do
    tCmdEnv <- envCmdEnv <$> ask
    liftIO $ atomically $ modifyTVar' tCmdEnv f

getMsg :: Shaun DiscordHandler (Maybe Message)
getMsg = do
    tCmdEnv <- envCmdEnv <$> ask
    cmdEnvMessage <$> liftIO (readTVarIO tCmdEnv)

setMsg :: Message -> Shaun DiscordHandler ()
setMsg msg = modEnv $ \ cmdEnv -> cmdEnv { cmdEnvMessage = Just msg }

getCmdEnv :: Shaun DiscordHandler CmdEnv
getCmdEnv = do
    tCmdEnv <- envCmdEnv <$> ask
    liftIO $ readTVarIO tCmdEnv
