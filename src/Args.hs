module Args (
    readArgsIO,
    ) where

import CustomPrelude

import System.Console.ParseArgs


readArgsIO :: IO (Args Text)
readArgsIO = parseArgsIO ArgsComplete argsList
  where
    argsList :: [Arg Text]
    argsList = [aToken, aConfig]

    -- File path to token file
    aToken = Arg
        { argIndex = "tokenFp"
        , argAbbr  = Nothing
        , argName  = Just "token"
        , argData  = argDataOptional "file path" ArgtypeString
        , argDesc  = "Path to token file."
        }

    -- File path to config file
    aConfig = Arg
        { argIndex = "configFp"
        , argAbbr = Nothing
        , argName = Just "config"
        , argData = argDataOptional "file path" ArgtypeString
        , argDesc = "Path to config file."
        }
