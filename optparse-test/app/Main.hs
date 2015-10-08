{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Options.Applicative.Args
import Options.Applicative.Builder.Extra
import Options.Applicative.Simple
import Options.Applicative.Types (readerAsk)
import Control.Exception
import System.Exit
import Debug.Trace
import Data.Monoid

data Command = Build deriving Show
data BuildOpts = BuildOpts {buildFoo :: Bool, innerGlobalOpts :: GlobalOpts} deriving Show
data GlobalOpts = GlobalOpts {globalFoo :: Bool, globalBar :: Bool} deriving (Show)

instance Monoid GlobalOpts where
  mempty = GlobalOpts {globalFoo = False, globalBar = False}
  mappend (GlobalOpts fooL barL) (GlobalOpts fooR barR) = 
    GlobalOpts ((||) fooL fooR) ((||) barL barR)

main :: IO ()
main = do
    traceIO "At main"
    eGlobalRun <-
        try $
        simpleOptions'
            "somethin1"
            "something2"
            "something3"
            globalParser
            (addCommand
                    "build"
                    "description of build"
                    buildCmd
                    (buildOptsParser Build))
    case eGlobalRun of
        Left (exitCode :: ExitCode) -> do
            throwIO exitCode
        Right (global,run) -> do
            run global


globalParser :: Parser GlobalOpts
globalParser =
    GlobalOpts <$> globalFoo <*> globalBar
  where
    globalFoo =
        boolFlags False "global-foo" "global foo flag" idm
    globalBar =
        boolFlags False "global-bar" "global bar flag" idm


buildOptsParser :: Command -> Parser BuildOpts
buildOptsParser _ =
  BuildOpts <$> buildFoo <*> globalParser
  where buildFoo =
          boolFlags
            False
            "build-foo"
            "build foo flag"
            idm

buildCmd :: BuildOpts -> GlobalOpts -> IO ()
buildCmd bOpts gOpts =
    print bOpts >> print gOpts >>
    return ()
