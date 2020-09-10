module Main
       ( main
       ) where

import Discord (RunDiscordOpts (..))
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

import qualified Data.Text as T
import qualified Discord

import qualified Bot
import qualified Env


main :: IO ()
main = do
    putTextLn "Starting shaunwhite..."

    putTextLn "Reading bot token..."
    botToken <- readFileText =<< getTokenPath

    -- Set up initial environment.
    initialEnv <- Env.initEnv

    let discordOpts :: RunDiscordOpts
        discordOpts =
            Discord.def { discordToken   = botToken
                        , discordOnEvent = Bot.eventHandler initialEnv
                        }

    putTextLn "Ready."
    userFacingError <- Discord.runDiscord discordOpts
    putTextLn userFacingError

    putStrLn "Finished."

  where
    getTokenPath :: IO FilePath
    getTokenPath = do
        homeDir <- getHomeDirectory

        let tokenLocations :: [FilePath]
            tokenLocations = [homeDir </> ".shaunwhite.token", "token"]

        searchFile tokenLocations
      where
        searchFile :: [FilePath] -> IO FilePath
        searchFile []          = do
            putTextLn "Could not find/read bot token."
            exitFailure
        searchFile (path : xs) = do
            putTextLn $ "Attempting to read token from `" <> T.pack path <> "`..."
            doesFileExist path >>= \case
                True  -> return path
                False -> searchFile xs
