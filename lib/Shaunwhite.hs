module Shaunwhite (
    runShaunwhite,
    ) where

import CustomPrelude

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
import Polysemy.State (State, runState)
import System.Console.ParseArgs (getArg)

import Args (readArgsIO)
import Config (readCfgFile, readTokenFile)
import Env (Env (..))
import Rolerequest (rolerequest)


{- | A bunch of required effects for boilerplate-y stuff, see:
http://www.morrowm.com/posts/2021-04-29-calamity.html
-}
type SetupEffects =
    C.SetupEff
        '[ParsePrefix C.Message
        , ConstructContext C.Message FullContext IO ()
        , MetricEff
        , CacheEff
        , Di Di.Level Di.Path Di.Message
        , P.Embed IO
        , P.Final IO
        ]

-- | Effects for additional stuff we want.
type ShaunwhiteEffects = '[State Env]

-- | Main entry point.
runShaunwhite :: IO ()
runShaunwhite = do
    -- Setup stuff
    -- TODO: Look into using polysemy for these IO actions as well. Might
    --       want to use a Reader with the command line arguments for example,
    --       if they are to be used elsewhere in the program.
    args <- readArgsIO
    shauntoken <- readTokenFile $ getArg args "tokenFp"
    _cfg <- readCfgFile
    env <- loadEnv  -- TODO: Load environment from configuration

    interpret shauntoken env eventHandlers
  where
    interpret :: C.Token -> Env -> P.Sem (ShaunwhiteEffects ++ SetupEffects) r -> IO ()
    interpret tok initialEnv handlers = Di.new $ \di ->
        void
        -- Handle all the boilerplate effects
        . P.runFinal
        . P.embedToFinal @IO
        . runDiToIO di
        . runCacheInMemory
        . runMetricsNoop
        . useFullContext
        . C.useConstantPrefix ">>="
        . C.runBotIO tok C.defaultIntents

        -- Handle additional effects we've added
        . runState initialEnv
        $ handlers

eventHandlers :: P.Sem (ShaunwhiteEffects ++ SetupEffects) ()
eventHandlers = do
    info @Text "Setting up event handlers..."

    -- Add bot commands
    void . C.addCommands $ do
        -- Show help
        void C.helpCommand

        -- Echo back the text following the command name
        void . C.command @'[Text] "echo" $ \ctxt txt -> do
            void $ C.tell ctxt txt

        -- Give the requested role to the user who issued the command.
        void $ C.command @'[Text] "rolerequest" rolerequest

    info @Text "Ready!"

-- TODO: Implement this for real and move into Env module.
--       Should probably have signature Cfg -> Env.
loadEnv :: IO Env
loadEnv = pure defaultEnv  -- TODO: read from config instead
  where
    defaultEnv :: Env
    defaultEnv = Env
        { _envAvailRoles =
            [ "DV2016"
            , "DV2017"
            , "DV2018"
            , "DV2019"
            , "DV2020"
            ]
        }
