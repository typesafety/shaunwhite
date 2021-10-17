module Args (
    readArgsIO,
    ) where

import CustomPrelude

import System.Console.ParseArgs


readArgsIO :: IO (Args Text)
readArgsIO = parseArgsIO ArgsComplete argsList
  where
    argsList :: [Arg Text]
    argsList = [aToken]

    aToken = Arg
        { argIndex = "tokenFp"
        , argAbbr  = Nothing
        , argName  = Just "token"
        , argData  = argDataOptional "printname" ArgtypeString
        , argDesc  = "Path to token file."
        }
