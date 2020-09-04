module Main
       ( main
       ) where

import Data.Aeson (decodeFileStrict')
import Discord (def, runDiscord, RunDiscordOpts (..))
import System.Directory (getXdgDirectory, XdgDirectory (XdgConfig))
import System.FilePath ((</>))
import System.IO.Error (catchIOError, IOError)

import Bot (eventHandler, Cfg (..))


main :: IO ()
main = do
    putTextLn "Starting shaunwhite..."

    putTextLn "Reading bot token..."
    botToken <- readFileText "token"

    -- Read config file to set up/restore initial environment.
    cfg <- getConfig

    let discordOpts :: RunDiscordOpts
        discordOpts =
            def { discordToken   = botToken
                , discordOnEvent = eventHandler cfg
                }

    putTextLn "Ready."
    userFacingError <- runDiscord discordOpts
    putTextLn userFacingError

    putStrLn "Finished."

getConfig :: IO Cfg
getConfig = do
    cfgDir <- getXdgDirectory XdgConfig "shaunwhite"
    let cfgLocation = cfgDir </> "config"
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

    defaultCfg :: Cfg
    defaultCfg = Cfg
        { cfgRequestableRoles = []
        }
