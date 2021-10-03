module Shaunwhite (
    runShaunwhite,
    ) where

import CustomPrelude

import Calamity qualified as C
import Calamity.Cache.Eff (CacheEff)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands qualified as C
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Commands.Dsl (hide)
import Calamity.Metrics.Eff (MetricEff)
import Calamity.Metrics.Noop (runMetricsNoop)
import CalamityCommands.Context (ConstructContext)
import CalamityCommands.ParsePrefix (ParsePrefix)
import Control.Exception (try)
import Di qualified
import DiPolysemy (Di, info, runDiToIO)
import Polysemy qualified as P
import Polysemy.State (State, runStateIORef)
import System.Console.ParseArgs (getArg)

import Args (readArgsIO)
import Auth (registerAdminCmd)
import Config (readCfgFile, readTokenFile)
import Env (Env (..), envFromCfg)
import Rolerequest (makeRequestable, rolerequest, revokeRequestable)


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
    -- TODO: Look into using proper effects for these IO actions as well.
    -- TODO: Look into using Di for this logging as well.
    -- TODO: Carry command-line arguments in a RO state.
    args <- readArgsIO
    shauntoken <- readTokenFile $ getArg args "tokenFp"
    envI <- initEnv
    env <- readIORef envI

    putTextLn $ "Starting shaunwhite with the following environment: \n" <> show env

    interpret shauntoken envI eventHandlers
  where
    interpret :: C.Token -> IORef Env -> P.Sem (ShaunwhiteEffects ++ SetupEffects) r -> IO ()
    interpret tok envI handlers = Di.new $ \di ->
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
        . runStateIORef envI
        $ handlers

    -- Initialize the starting environment using a config file if successful,
    -- using a default environment otherwise.
    initEnv :: IO (IORef Env)
    initEnv = do
        env <- try @SomeException readCfgFile <&> \case
            Right cfg -> envFromCfg cfg
            Left _    -> defaultEnv
        newIORef env
      where
        defaultEnv :: Env
        defaultEnv = Env
            { _envRequestableRoles = mempty
            }


eventHandlers :: P.Sem (ShaunwhiteEffects ++ SetupEffects) ()
eventHandlers = do
    info @Text "Setting up event handlers..."

    {- TODO: Implement some proper wrapping system for registering commands to
    lessen the risk of accidentally allowing anyone to issue admin commands.
    -}
    -- Register bot commands.
    void $ C.addCommands $ do
        -- Show help
        void C.helpCommand

        -- Echo back the text following the command name
        void $ C.command @'[Text] "echo" $ \ctxt txt -> do
            void $ C.tell ctxt txt

        -- Give the requested role to the user who issued the command.
        void $ C.command @'[Text] "rolerequest" rolerequest

        -- Make a role requestable. This command requires admin.
        registerAdminCmd $ hide $ C.command @'[Text] "makeRequestable" makeRequestable

        -- Revoke requestable status from a role. This command requires admin.
        registerAdminCmd . hide . C.command @'[Text] "revokeRequestable"
            $ \_ roleName ->  revokeRequestable roleName

    info @Text "Ready!"
