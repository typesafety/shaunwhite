{-# LANGUAGE TemplateHaskell #-}

module Config (
    Cfg (Cfg),

    -- * The program state while running
    Env (..),
    envAvailRoles,

    getToken,
    ) where

import CustomPrelude

import Calamity.Types.Token (Token (BotToken))
import Control.Lens.TH (makeLenses)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import System.Directory
  ( XdgDirectory (XdgConfig),
    doesFileExist,
    getXdgDirectory,
  )
import System.FilePath ((<.>), (</>))


-- | Contains necessary state for the program while running.
data Env = Env
    { _envAvailRoles :: [L.Text]
    -- ^ Names of roles that can be requested.
    -- TODO: possibly use some other means of identification of roles, such
    --       as the role ID (:: Snowflake Role).
    } deriving (Show)
$(makeLenses ''Env)

-- | Persistent data to be read into the runtime environment at bot startup.
data Cfg = Cfg
    { _cfgAvailRoles :: [L.Text]
    } deriving (Show)

{- | Attempt to locate and read the bot token.

The first token found in one of the following locations is used, in order:

1. `--token` command line parameter. If the provided file does not exist, the program
    will exit.
2. $XDG_CONFIG_HOME/shaunwhite/shaunwhite.token
-}
getToken :: Maybe FilePath -> IO Token
getToken = \case
    Just explicitFp -> doesFileExist explicitFp >>= \case
        True  -> makeBotToken <$> readFileText explicitFp
        False -> fail $ "Token not found at: `" <> explicitFp <> "`"
    Nothing -> do
        cfgDir <- getXdgDirectory XdgConfig configSubdir
        token <- readFile $ cfgDir </> "shaunwhite" <.> "token"
        pure . makeBotToken . fromString $ token
  where
    configSubdir = "shaunwhite"

    makeBotToken :: Text -> Token
    makeBotToken = BotToken . toLazy . T.strip
