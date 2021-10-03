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
import Control.Lens
import Di qualified
import DiPolysemy (Di, info, runDiToIO)
import Polysemy qualified as P
import Polysemy.Reader (Reader, runReader)
import System.Console.ParseArgs (getArg)

import Args (readArgsIO)
import Auth (registerAdminCmd)
import Config (readCfgFile, readTokenFile)
import Env (Env (..), Roe (..), envFromCfg, roeEnv)
import Rolerequest (makeRequestable, rolerequest)



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
type ShaunwhiteEffects = '[Reader Roe]

-- | Main entry point.
runShaunwhite :: IO ()
runShaunwhite = do
    -- Setup stuff
    -- TODO: Look into using polysemy for these IO actions as well. Might
    --       want to use a State with the command line arguments for example,
    --       if they are to be used elsewhere in the program.
    --       Use proper Di logging.
    args <- readArgsIO
    shauntoken <- readTokenFile $ getArg args "tokenFp"
    roe <- initRoe

    env <- readIORef $ view roeEnv roe
    putTextLn $ "Starting shaunwhite with the following environment: \n" <> show env

    interpret shauntoken roe eventHandlers
  where
    interpret :: C.Token -> Roe -> P.Sem (ShaunwhiteEffects ++ SetupEffects) r -> IO ()
    interpret tok initialROE handlers = Di.new $ \di ->
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
        . runReader initialROE
        $ handlers

    -- Initialize the starting environment using a config file if successful,
    -- using a default environment otherwise.
    initRoe :: IO Roe
    initRoe = do
        env <- try @SomeException readCfgFile <&> \case
            Right cfg -> envFromCfg cfg
            Left _    -> defaultEnv
        envIor <- newIORef env
        pure $ Roe
            { _roeEnv = envIor
            }
      where
        defaultEnv :: Env
        defaultEnv = Env
            { _envAvailRoles = []
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

    info @Text "Ready!"
