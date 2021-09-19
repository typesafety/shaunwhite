module Shaunwhite (
    runShaunwhite,
    ) where

import Relude

import Calamity qualified as C
import Calamity.Cache.Eff (CacheEff)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands qualified as C
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Metrics.Noop (runMetricsNoop)
import CalamityCommands.Context (ConstructContext)
import CalamityCommands.ParsePrefix (ParsePrefix)
import Di qualified
import DiPolysemy (Di, info, runDiToIO)
import Polysemy qualified as P

import System.Console.ParseArgs (getArg)
import Args (readArgsIO)
import Config
import Rolerequest (rolerequest)


type ShaunwhiteEffects =
    C.SetupEff
        '[ParsePrefix C.Message
        , ConstructContext C.Message FullContext IO ()
        , MetricEff
        , CacheEff
        , Di Di.Level Di.Path Di.Message
        , P.Embed IO
        , P.Final IO
        ]

runShaunwhite :: IO ()
runShaunwhite = do
    -- Setup stuff
    args <- readArgsIO
    shauntoken <- getToken $ getArg args "tokenFp"
    _cfg <- loadCfg  -- TODO: Implement config loading when needed

    interpret shauntoken eventHandlers
  where
    interpret :: C.Token -> P.Sem ShaunwhiteEffects r -> IO ()
    interpret tok handlers = Di.new $ \di ->
        void
        . P.runFinal
        . P.embedToFinal @IO
        . runDiToIO di
        . runCacheInMemory
        . runMetricsNoop
        . useFullContext
        . C.useConstantPrefix ">>="
        . C.runBotIO tok C.defaultIntents
        $ handlers

eventHandlers :: P.Sem ShaunwhiteEffects ()
eventHandlers = do
    info @Text "Setting up event handlers..."

    -- Add bot commands
    void . C.addCommands $ do
        -- Show help
        void C.helpCommand

        -- Echo back the text following the command name
        void $ C.command @'[Text] "echo" $ \ctxt txt -> do
            void $ C.tell ctxt txt

        -- Give the requested role to the user who issued the command.
        void $ C.command @'[Text] "rolerequest" rolerequest

    info @Text "Ready!"

loadCfg :: IO Cfg
loadCfg = pure $ Cfg []  -- TODO: Do something
