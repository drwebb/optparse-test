module Main where

import Test.Hspec
import System.IO
import System.Process
import System.Exit
import Control.Monad.IO.Class

main :: IO ()


main =
    hspec $
    before stackBuild $
    do describe "Optparse tests" $
           do it "No options case" $
                  do exit <- runExe [""]
                     do exit `shouldBe` ExitSuccess
              it "foo base command case" $
                  do exit <- runExe ["foo"]
                     do exit `shouldBe` ExitSuccess
              it "foo base command case" $
                  do exit <- runExe ["--bar", "foo"]
                     do exit `shouldBe` ExitSuccess

stackBuild :: IO ()
stackBuild =
    callProcess
        "stack"
        ["build"]

runExe :: [String] -> IO ExitCode
runExe options = do
    (exitCode,_,_) <-
        readProcessWithExitCode
            "stack"
            (["exec", "--", "optparse-test-exe"] ++
             options) ""
    return exitCode
