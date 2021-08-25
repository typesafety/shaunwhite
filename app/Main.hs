module Main
    ( main
    ) where

import Relude

import Calamity
import Calamity.Cache.Eff
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context
import Calamity.Metrics.Eff
import Calamity.Metrics.Noop
import CalamityCommands.Context
import CalamityCommands.ParsePrefix (ParsePrefix)
import Di qualified
import DiPolysemy
import Polysemy qualified as P
import System.Console.ParseArgs

import Utils.Config


main :: IO ()
main = do
    -- Setup stuff
    args <- getArgs
    shauntoken <- getToken $ getArg args "tokenFp"
    cfg <- loadCfg

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
        . runBotIO tok defaultIntents
        $ handlers

eventHandlers ::
    P.Sem
        (SetupEff
            '[ParsePrefix Message,
            ConstructContext Message FullContext IO (),
            MetricEff,
            CacheEff,
            Di Di.Level Di.Path Di.Message,
            P.Embed IO,
            P.Final IO])
        ()
eventHandlers = do
    info @Text "Setting up event handlers..."
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
