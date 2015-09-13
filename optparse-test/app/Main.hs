{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative.Args
import Options.Applicative.Builder.Extra
import Options.Applicative.Simple
import Options.Applicative.Types (readerAsk)
import Control.Exception
import System.Exit

data Command = Build
data BuildOpts = BuildOpts {buildFoo :: String}
data GlobalOpts = GlobalOpts {globalFoo :: String}

main :: IO ()
main = do
    eGlobalRun <-
        try $
        simpleOptions
            "something1"
            "something2"
            "something3"
            globalParser
            (do addCommand
                    "build"
                    "description of build"
                    buildCmd
                    (buildOptsParser Build))
    case eGlobalRun of
        Left (exitCode :: ExitCode) -> do
            throwIO exitCode
        Right (global,run) -> do
            print "sucessfulRun"

globalParser :: Parser GlobalOpts
globalParser = undefined

buildOptsParser :: Command -> Parser BuildOpts
buildOptsParser _ = undefined

buildCmd :: BuildOpts -> GlobalOpts -> IO ()
buildCmd _ _ = return ()
