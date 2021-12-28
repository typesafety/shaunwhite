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
import Control.Exception (try)
import Data.Flags ((.+.))
import Di qualified
import DiPolysemy (Di, info, runDiToIO)
import Polysemy qualified as P
import Polysemy.State (State, runStateIORef)
import System.Console.ParseArgs (getArg)

import Args (readArgsIO)
import Config (readCfgFile, readTokenFile)
import Env (Env (..), envFromCfg)
import Roles (registerRolesCommands, rolerequest)


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
        . C.runBotIO tok allIntents

        -- Handle additional effects we've added
        . runStateIORef envI
        $ handlers
      where
        -- Requires privileged gateway intents to be enabled for the bot,
        -- see: https://discord.com/developers/docs/topics/gateway#gateway-intents
        -- Enable at the developer portal page for the bot:
        -- https://discord.com/developers/applications/[APPLICATION_ID]/bot
        allIntents :: C.Intents
        allIntents = C.defaultIntents .+. C.intentGuildMembers .+. C.intentGuildPresences

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
    (_removeHandler, _handler, _) <- C.addCommands $ do
        -- Show help
        -- TODO: Write better custom help command
        void C.helpCommand

        -- Echo back the text following the command name
        void $ C.command @'[Text] "echo" $ \ctxt txt -> do
            void $ C.tell ctxt txt

        -- Register commands for adding/removing/requesting roles etc.
        registerRolesCommands
        -- Add old command for backwards compatibility
        void $ C.hide $ C.command @'[Text] "rolerequest" rolerequest

    info @Text "Ready!"
