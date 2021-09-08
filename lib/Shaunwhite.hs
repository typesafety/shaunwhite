module Shaunwhite (
    runShaunwhite,
    ) where

import Relude

import Calamity qualified as C
import Calamity.Cache.Eff
import Calamity.Cache.InMemory
import Calamity.Commands as C
import Calamity.Commands.Context
import Calamity.Metrics.Eff
import Calamity.Metrics.Noop
import CalamityCommands.Context
import CalamityCommands.ParsePrefix (ParsePrefix)
import Di qualified
import DiPolysemy
import Polysemy qualified as P
import System.Console.ParseArgs

import Config


runShaunwhite :: IO ()
runShaunwhite = do
    -- Setup stuff
    args <- getArgs
    shauntoken <- getToken $ getArg args "tokenFp"
    _cfg <- loadCfg  -- TODO: Implement config loading when needed

    interpret shauntoken eventHandlers
  where
    interpret tok handlers = Di.new $ \di ->
        void
        . P.runFinal
        . P.embedToFinal @IO
        . runDiToIO di
        . runCacheInMemory
        . runMetricsNoop
        . useFullContext
        . useConstantPrefix ">>="
        . C.runBotIO tok C.defaultIntents
        $ handlers

eventHandlers ::
    P.Sem
        (C.SetupEff
            '[ParsePrefix C.Message,
            ConstructContext C.Message FullContext IO (),
            MetricEff,
            CacheEff,
            Di Di.Level Di.Path Di.Message,
            P.Embed IO,
            P.Final IO])
        ()
eventHandlers = do
    info @Text "Setting up event handlers..."

    -- Add bot commands
    void . addCommands $ do
        -- Show help
        void helpCommand

        -- Echo back the text following the command name
        void $ C.command @'[Text] "echo" $ \ctxt txt -> do
            void $ C.tell ctxt txt

    info @Text "Ready!"

getArgs :: IO (Args Text)
getArgs = parseArgsIO ArgsComplete argsList
  where
    argsList = [
        Arg {
            argIndex = "tokenFp",
            argAbbr  = Nothing,
            argName  = Just "token",
            argData  = argDataOptional "printname" ArgtypeString,
            argDesc  = "Path to token file."
        }
        ]

loadCfg :: IO Cfg
loadCfg = pure $ Cfg []  -- TODO: Do something