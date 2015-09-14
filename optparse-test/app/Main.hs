{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative.Args
import Options.Applicative.Builder.Extra
import Options.Applicative.Simple
import Options.Applicative.Types (readerAsk)
import Control.Exception
import System.Exit

data Command = Build
data BuildOpts = BuildOpts {buildFoo :: Bool} deriving Show
data GlobalOpts = GlobalOpts {globalFoo :: Bool} deriving Show

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
            print "Sucessful Run, options are"
            

globalParser :: Parser GlobalOpts
globalParser = 
  GlobalOpts <$> globalFoo
  where globalFoo =
          boolFlags
            False
            "global-foo"
            "global foo flag"
            idm

buildOptsParser :: Command -> Parser BuildOpts
buildOptsParser _ =
  BuildOpts <$> buildFoo
  where buildFoo =
          boolFlags
            False
            "build-foo"
            "build foo flag"
            idm

buildCmd :: BuildOpts -> GlobalOpts -> IO ()
buildCmd _ _ = return ()
