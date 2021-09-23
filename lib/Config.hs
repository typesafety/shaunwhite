{-# LANGUAGE TemplateHaskell #-}

{- | Module that concerns storing and loading persistent data into the bot for
use while running, such as the bot token or configuration settings.
-}

module Config (
    Cfg (Cfg),
    cfgAvailRoles,

    readCfgFile,
    readTokenFile,
    ) where

import CustomPrelude

import Calamity.Types.Token (Token (BotToken))
import Control.Lens.TH (makeLenses)
import Data.Aeson
  ( FromJSON,
    ToJSON,
    decodeFileStrict',
    object,
    parseJSON,
    toJSON,
    withObject,
    (.:),
    (.=),
  )
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import System.Directory
  ( XdgDirectory (XdgConfig),
    doesFileExist,
    getXdgDirectory,
  )
import System.FilePath ((<.>), (</>))


-- | Persistent data to be read into the runtime environment at bot startup.
data Cfg = Cfg
    { _cfgAvailRoles :: [L.Text]
    } deriving (Show)
$(makeLenses ''Cfg)

instance FromJSON Cfg where
    parseJSON = withObject "Cfg" $ \v -> Cfg
        <$> v .: "requestable_roles"

instance ToJSON Cfg where
    toJSON (Cfg availRoles) =
        object ["requestable_roles" .= availRoles]

{- | Attempt to read a Cfg from file. Expects a JSON format, and raises an
exception if encountering issues, e.g. file not found or in the wrong format.

The first of the following options will be attempted (failure will not cause
following options to be attempted):

(TODO: Not implemented yet) 1. @--token@ command line parameter.
2. @XDG_CONFIG_HOME/shaunwhite/config.json@
-}
readCfgFile :: IO Cfg
readCfgFile = do
    cfgDir <- getCfgDir
    Just cfgJson <- decodeFileStrict' $ cfgDir </> "config" <.> "json"
    pure cfgJson

{- | Attempt to locate and read the bot token.

The first token found in one of the following locations is used, in order:

1. `--token` command line parameter. If the provided file does not exist, the program
    will exit.
2. $XDG_CONFIG_HOME/shaunwhite/shaunwhite.token
-}
readTokenFile :: Maybe FilePath -> IO Token
readTokenFile = \case
    Just explicitFp -> doesFileExist explicitFp >>= \case
        True  -> makeBotToken <$> readFileText explicitFp
        False -> fail $ "Token not found at: `" <> explicitFp <> "`"
    Nothing -> do
        cfgDir <- getCfgDir
        token <- readFile $ cfgDir </> "shaunwhite" <.> "token"
        pure . makeBotToken . fromString $ token
  where
    makeBotToken :: Text -> Token
    makeBotToken = BotToken . toLazy . T.strip

getCfgDir :: IO FilePath
getCfgDir = getXdgDirectory XdgConfig configSubdir
  where
    configSubdir = "shaunwhite"

