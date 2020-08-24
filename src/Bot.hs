{-# LANGUAGE LambdaCase #-}

module Bot
       ( eventHandler
       ) where

import Discord (DiscordHandler)
import Discord.Types (Event (..))

import Calls.Echo (cmdEcho)
import Commands (Cmd (..), cmdFromMessage)


eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate msg) = whenJust (cmdFromMessage msg) execCmd
eventHandler _ = pass

execCmd :: Cmd -> DiscordHandler ()
execCmd = \case
    CmdEcho msg -> do
        res <- cmdEcho msg
        whenLeft_ res print
    _ -> do
        -- TODO: Behavior for this (impossible) case?
        putTextLn "execCmd: unrecognized command"
        putTextLn "pls fix."
