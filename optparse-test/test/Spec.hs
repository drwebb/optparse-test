module Main where

import Test.Hspec
import System.Process
import System.Exit

main :: IO ()

main =
    hspec $
    before stackBuild $
    do describe "Optparse tests" $
           do it "No options case" $
                  do exit <- runExe [""]
                     do exit `shouldBe` ExitSuccess
              it "foocmd" $
                  do exit <- runExe ["foocmd"]
                     do exit `shouldBe` ExitSuccess
              it "--bar foocmd" $
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
