module Utils.Config
    ( getToken
    ) where

import           Relude

import           Calamity.Types.Token (Token (BotToken))
import           System.Directory     (XdgDirectory (XdgConfig), doesFileExist,
                                       getXdgDirectory)
import           System.FilePath      ((<.>), (</>))


{- | Attempt to locate and read the bot token.

The first token found in one of the following locations is used, in order:

1. `--token` command line parameter. If the provided file does not exist, the program
    will exit.
2. $XDG_CONFIG_HOME/shaunwhite/shaunwhite.token

-}
getToken :: Maybe FilePath -> IO Token
getToken (Just explicitFp) = doesFileExist explicitFp >>= \case
    True  -> BotToken . fromStrict <$> readFileText explicitFp
    False -> fail $ "Token not found at: `" <> explicitFp <> "`"
getToken Nothing = do
    cfgDir <- getXdgDirectory XdgConfig configSubdir
    token <- readFile $ cfgDir </> "shaunwhite" <.> "token"
    pure . BotToken . fromString $ token
  where
    configSubdir = "shaunwhite"
