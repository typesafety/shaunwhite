module Main
       ( main
       ) where

import Discord (def, runDiscord, RunDiscordOpts (..))

import Bot (eventHandler, getConfig)


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

