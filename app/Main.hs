module Main
       ( main
       ) where

import Discord (def, runDiscord, RunDiscordOpts (..))

import Bot (eventHandler)


main :: IO ()
main = do
    putTextLn "Starting shaunwhite..."

    putTextLn "Reading bot token..."
    botToken <- readFileText "token"

    putTextLn "Ready."
    res <- runDiscord (discordOpts botToken)
    putTextLn res

    putStrLn "Finished."

  where
    discordOpts :: Text -> RunDiscordOpts
    discordOpts botToken =
        def { discordToken   = botToken
            , discordOnEvent = eventHandler
            }
