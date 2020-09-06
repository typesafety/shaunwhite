module Main
       ( main
       ) where

import Discord (def, runDiscord, RunDiscordOpts (..))

import qualified Bot
import qualified Env


main :: IO ()
main = do
    putTextLn "Starting shaunwhite..."

    putTextLn "Reading bot token..."
    botToken <- readFileText "token"

    -- Set up initial environment.
    initialEnv <- Env.initEnv

    let discordOpts :: RunDiscordOpts
        discordOpts =
            def { discordToken   = botToken
                , discordOnEvent = Bot.eventHandler initialEnv
                }

    putTextLn "Ready."
    userFacingError <- runDiscord discordOpts
    putTextLn userFacingError

    putStrLn "Finished."
