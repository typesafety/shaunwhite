module Main
    ( main
    ) where

import           Relude

import           System.Console.ParseArgs

import           Utils.Config


main :: IO ()
main = do
    args <- getArgs
    token <- getToken $ getArg args "tokenFp"

    print token

getArgs ::IO (Args Text)
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
